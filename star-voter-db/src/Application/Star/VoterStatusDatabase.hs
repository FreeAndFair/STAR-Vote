{-# LANGUAGE DeriveDataTypeable,
             FlexibleContexts,
             OverloadedStrings,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeFamilies       #-}
module Application.Star.VoterStatusDatabase where

import Application.Star.Templates
import Application.Star.VoterStatusForms

import Application.Star.CommonImports
import Application.Star.ID
import Application.Star.Precinct
import Application.Star.Util hiding (method)

import Application.Star.BallotStyle
import Application.Star.Voter

import Application.Star.VoterStatusSticker (sticker)

import Data.Acid
import Data.SafeCopy

import Control.Lens (over, set, view, _1, _2, _3, _4)
import Control.Lens.TH

import Control.Monad.Trans.Maybe

import Control.Arrow
import Data.List
import Data.Foldable (foldMap)
import Data.Traversable
import Numeric

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Csv        as CSV
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Vector (Vector)
import Text.Blaze.Html5 ((!))

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Snap hiding (method)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Paths_star_voter_db (getDataFileName)

import Debug.Trace

-- Assumptions:
--
--   (1) Each voter gets to vote exactly once
--
--   (2) Voters will be registered in n precincts. Typically, n=1, but
--       if they have recently moved, then n might be larger. If they
--       are not registered in any precincts, n=0 and they can't vote.
--
--   (3) Each precinct registration will give exactly one ballot style

-- The interface consists of three possible requests. Below we give the HTTP
-- method and path that should be used, together with any parameters that it
-- takes marked by either "URI" for parameters that should go in the URI or
-- "Body" for parameters that should go in the request body. The parameters are
-- decoded using UTF8 then parsed using read. The Read and Show instance for
-- (ID x) is not the derived one; it is the Integer instance.
-- GET   lookup           :: { voter :: URI  (ID Voter)                        } -> Maybe (VoterStatus, [ID Precinct])
-- POST  initialize       :: { db    :: Body [(ID Voter, ID Precinct)]         } -> {- produces the empty string as output -}
-- PATCH atomicSwapStatus :: { voter :: Body (ID Voter), status :: Body Status } -> Status

----------------------
-- Database definition
----------------------



-- | The actual database
data StatusDB = StatusDB
  { _statusDB :: M.Map (ID Voter) (VoterStatus, Voter, [(ID Precinct, BallotStyleId)])
  , _provisionalVoters :: [(Voter, ID Precinct, BallotStyleId)] }
  deriving Show

$(deriveSafeCopy 0 'base ''StatusDB)
$(makeLenses ''StatusDB)

-----------------------------
-- Operations on the database
-----------------------------

lookupVoter :: ID Voter
            -> Query StatusDB
                     (Maybe (VoterStatus, Voter, [(ID Precinct, BallotStyleId)]))
lookupVoter voterId = do db <- view statusDB <$> ask
                         return $ M.lookup voterId db

-- | Search in the text fields of the voter DB for a string
searchVoter :: T.Text -> Query StatusDB [(ID Voter, Int, Voter, VoterStatus)]
searchVoter q = do db <- view statusDB <$> ask
                   let scored = M.map (score . (view _2 &&& view _1)) $ db
                   return . sortBy comp . filter ((>0) . (view _2)) . map flat $  M.toList scored

  where score v = (sum [ T.count q' (T.toLower $ view (_1 . voterName) v) +
                         T.count q' (T.toLower $ view (_1 . voterAddress) v)
                       | q' <- qs
                       ], v)
        comp (_, sc1, _, _) (_, sc2, _, _) = opposite $ compare sc1 sc2
        flat (w, (x, (y, z))) = (w, x, y, z)
        qs = T.words $ T.toLower q
        opposite LT = GT
        opposite EQ = EQ
        opposite GT = LT

validVoters :: Query StatusDB [ID Voter]
validVoters = M.keys . view statusDB <$> ask

addProvisional :: Voter -> ID Precinct -> BallotStyleId -> Update StatusDB ()
addProvisional v prec bsid = modify (over provisionalVoters ((v, prec, bsid) :))

initializeDB :: StatusDB -> Update StatusDB ()
initializeDB = put

markVoted :: ID Voter -> Update StatusDB ()
markVoted = modify . over statusDB . M.adjust (set _1 Voted)

$(makeAcidic ''StatusDB [ 'lookupVoter
                        , 'validVoters
                        , 'initializeDB
                        , 'addProvisional
                        , 'markVoted
                        , 'searchVoter
                        ])

--------------------
-- Building a new DB
--------------------

buildStatusDB :: [(ID Voter, Voter)]
              -> [(ID Voter, ID Precinct, BallotStyleId)]
              -> StatusDB
buildStatusDB ids precs = flip StatusDB [] . M.mapMaybe flatten .
                                             M.unionWith combine initial .
                                             M.fromListWith combine . map inject $ precs
  where
    combine (_, v, pbs1) (_, _, pbs2) = (Hasn't, v, pbs1 <> pbs2)
    inject (voter, precinct, style) = (voter, (Hasn't, Nothing, [(precinct, style)]))
    initial = M.fromList $ map (\(vid, v) -> (vid, (Hasn't, Just v, []))) ids
    flatten (status, Just voter, precs) = Just (status, voter, precs)
    flatten (_, Nothing, _) = Nothing

openDB :: (MonadIO m) => m (AcidState StatusDB)
openDB = do name <- liftIO (getDataFileName "voterdb")
            liftIO $ flip openLocalStateFrom (StatusDB M.empty []) name


doCheckpoint :: (MonadIO m, MonadAcidState s m) => m ()
doCheckpoint = get >>= liftIO . createCheckpoint

fakeData :: IO ()
fakeData = do getDataFileName "voterdb" >>= putStrLn . ("Opening " ++)
              db <- openDB
              let fake = StatusDB (M.fromList [ (1, (Hasn't, david, [(1, "ballot1")]))
                                              , (2, (Hasn't, mr_rogers, [(1, "ballot2")]))
                                              ])
                                  []
              update db (InitializeDB fake)
              createCheckpoint db
              closeAcidState db

  where david = Voter "David Raymond Christiansen" "Saltholmsvej 1, 2. th\n2300 København S\nDanmark"
        mr_rogers = Voter "Mr Rogers" "PBS"

main :: IO ()
main = do getDataFileName "voterdb" >>= putStrLn . ("Opening " ++)
          db <- openDB
          statefulErrorServe voterStatusDB db
          closeAcidState db

index :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
index = render . pageHtml . starPageWithContents "Voter Check-In" $
          H.ul $ do
            H.li $ H.a ! A.href "/search" $ "Find voters"
            H.li $ H.a ! A.href "/add-provisional" $ "Register provisional voter"

search :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
search = do
  trace "here" $ return ()
  q <- getParam "q"
  res <- traverse (doQuery . SearchVoter . decodeUtf8) q
  let resultTable = 
        case res of
          Nothing -> mempty
          Just [] -> "Nothing found"
          Just xs -> H.table ! A.class_ "table table-striped" $ do
                       header
                       mconcat $ map row xs
            where header = H.tr $ mconcat [ H.th "Name"
                                          , H.th "Status"
                                          , H.th "Address"
                                          , H.th mempty
                                          ]
                  row (vid, a, v, status) =
                    H.tr . mconcat . map H.td $
                      [ H.toHtml $ view voterName v
                      , case status of
                          Hasn't -> "Hasn't checked in"
                          Voted -> "Has checked in"
                      , H.pre . H.toHtml $ view voterAddress v
                      , H.ul . mconcat . map H.li $
                          [ H.a ! A.href (H.toValue $ "/sticker?voter=" ++ show vid) $
                              "sticker"
                          , H.a ! A.href (H.toValue $ "/mark-voted?voter=" ++ show vid) $
                              "check in"
                          ]
                      ]
  render . pageHtml .
    starPageWithContents "Search" $ do
      H.form ! A.method "GET" $ do
        H.input ! A.name "q" ! A.type_ "text" ! A.value (H.toValue $ maybe "" decodeUtf8 q)
        H.input ! A.type_ "submit"
      resultTable

-- | Print the barcode sticker for a voter, given an ID from the URL
stickerPage :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
stickerPage = do
  voter <- readURIParam "voter"
  v <- doQuery $ LookupVoter voter
  case v of
    Nothing -> do modifyResponse $ setResponseStatus 404 "Not found"
                  writeLBS "Voter not found"
    Just (_, v, []) -> writeLBS "oops, no precinct!"
    Just (_, v, (prec,bsid):_) -> do modifyResponse $ setContentType "application/pdf"
                                     stickerPDF <- liftIO $ sticker False v prec bsid
                                     writeLBS stickerPDF
                                     
-- | Print the barcode sticker for a provisional voter, given their information
provisionalStickerPage :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
provisionalStickerPage = do
  voter <- Voter <$> (decodeUtf8 <$> requireParam "name") <*>
                     (decodeUtf8 <$> requireParam "address")
  precinct <- readURIParam "precinct"
  ballotStyle <- maybe (error "no ballot style") decodeUtf8 <$> getParam "ballotstyle"
  modifyResponse $ setContentType "application/pdf"
  stickerPDF <- liftIO $ sticker True voter precinct ballotStyle
  writeLBS stickerPDF


-- | A handler that displays and processes a form for provisional voter registration
addProvisionalForm :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
addProvisionalForm = do -- Form for adding provisional voters
  (view, res) <- runForm "provisional" $
                   liftA3 (\a b c -> (a,b,c)) voterForm idForm ballotStyleForm
  case res of
    Nothing ->  -- input not recieved
      render . pageHtml . starPageWithContents "Provisional registration" $ do
        form view "add-provisional" ! A.class_ "form-horizontal row" $ do
          voterView view
          idView "Precinct" view
          ballotStyleView view
          H.input ! A.type_ "submit"
    Just (voter@(Voter name address), precinct, ballotStyle) -> do -- input received
      doUpdate $ AddProvisional voter precinct ballotStyle
      let stickerUrl = "provisional-sticker?" <>
                       printUrlEncoded (M.fromList [ ("name", [encodeUtf8 name])
                                                   , ("address", [encodeUtf8 address])
                                                   , ("precinct", [encodeUtf8 . T.pack $ show precinct])
                                                   , ("ballotstyle", [encodeUtf8 ballotStyle])
                                                   ])
      render . pageHtml . starPageWithContents ("Provisional registration: " <> name) $ do
        H.p $
          H.toHtml name <>
          " has been registered as a provisional voter with the " <>
          "following information:"
        H.dl ! A.class_ "dl-horizontal" $ 
          mconcat . map (\(h, c) -> H.dt h <> H.dd c) $
            [ ("Name", H.toHtml name)
            , ("Address", H.pre (H.toHtml address))
            , ("Precinct", H.toHtml $ show precinct)
            , ("Ballot style", H.toHtml ballotStyle)
            ]
        H.ul $ do 
          H.li $ H.a ! A.href (H.toValue (decodeUtf8 stickerUrl)) $ "Print sticker"
          H.li $ H.a ! A.href "/" $ "Return"

alreadyVoted name address vid = 
  render . pageHtml . starPageWithContents (name <> " has already checked in!") $ do
    "Name: " <> H.toHtml name
    H.br
    "Address:" <> H.pre (H.toHtml address)
    H.br
    H.ul . mconcat . map H.li $
      [ H.a ! A.href (H.toValue $ "/sticker?voter=" ++ show vid) $ "Re-print sticker"
      , H.a ! A.href "/search" $ "Return"
      ]

markVotedConfirm :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
markVotedConfirm = do
  vid  <- readURIParam "voter"
  voter <- doQuery (LookupVoter vid)
  case voter of
    Nothing -> render . pageHtml . starPageWithContents "Invalid voter ID" $
                 "The voter ID " <> (H.toHtml $ show vid) <> "is invalid."
    Just (Hasn't, Voter name address, _) ->
      render . pageHtml . starPageWithContents "Voter Check-In: " $
        H.form ! A.method "POST" $ do
          H.input ! A.type_ "hidden" ! A.name "voter" ! A.value (H.toValue (show vid))
          "Name: " <> H.toHtml name
          "Address:" <> H.pre (H.toHtml address)
          H.input ! A.type_ "submit" ! A.value "Confirm check-in"
    Just (Voted, Voter name address, _) -> alreadyVoted name address vid

markVotedPage :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
markVotedPage = do
  r <- getRequest
  liftIO $ putStrLn (show (rqPostParams r))
  vid  <- readBodyParam "voter"
  voter <- doQuery (LookupVoter vid)
  case voter of
    Nothing -> render . pageHtml . starPageWithContents "Invalid voter ID" $
                 "The voter ID " <> (H.toHtml $ show vid) <> "is invalid."
    Just (Voted, Voter name address, _) -> alreadyVoted name address vid
    Just (Hasn't, Voter name address, _) ->
      do doUpdate (MarkVoted vid)
         render . pageHtml . starPageWithContents "Check-in successful!" $ do
           H.p $ do H.toHtml name <> " has checked in successfully. Remember to print the "
                    H.a ! A.href (H.toValue $ "/sticker?voter=" ++ show vid) $ "sticker"
                    "."
           H.p $ H.a ! A.href "/search" $ "Another"

instance CSV.FromField (ID a) where parseField f = ID <$> CSV.parseField f

initializePage :: MonadSnap m => m ()
initializePage = render . pageHtml . starPageWithContents "Voter Database Initialization" $ do
  H.form ! A.method "post" $ do
    H.p instructions
    H.textarea ! A.cols "80" ! A.rows "10" ! A.name "csv" $ defaultDB
    H.br
    H.button "Submit"
  where
  instructions = "Enter voters in CSV format; the columns are voter ID, name, address, precinct ID, and ballot style (in that order)."
  defaultDB = "1,John Doe,Nowhereland,1,oregon-2014\n2,Jane Doe,Stix,2,oregon-2014\n"

buildDBFromCSV :: Vector (ID Voter, Text, Text, ID Precinct, BallotStyleId) -> StatusDB
buildDBFromCSV csv = buildStatusDB (foldMap getVoter csv) (foldMap getBallot csv) where
  getVoter (voterID, name, address, _, _) = [(voterID, Voter name address)]
  getBallot (voterID, _, _, precinctID, ballotStyleID) = [(voterID, precinctID, ballotStyleID)]

voterStatusDB :: (MonadSnap m, MonadError Text m, MonadAcidState StatusDB m) => m ()
voterStatusDB =
  ifTop index <|>
  route
    [ ("initialize", method POST $ do
      csv_ <- getParam "csv"
      case csv_ of
        Nothing  -> writeShow "expecting some CSV data in the csv parameter"
        Just csv -> case CSV.decode CSV.NoHeader (LBS.fromStrict csv) of
          Left err -> writeShow err
          Right db -> doUpdate . InitializeDB . buildDBFromCSV $ db
      )
    , ("initialize", method GET initializePage)
    , ("search", method GET search)
    , ("sticker", method GET stickerPage)
    , ("add-provisional", addProvisionalForm)
    , ("provisional-sticker", method GET provisionalStickerPage)
    , ("mark-voted", method GET markVotedConfirm <|> method POST markVotedPage)
    ]

requireParam :: MonadSnap m => ByteString -> m ByteString
requireParam name = maybe (error $ "Parameter " ++ show name ++ " not provided.") id <$> getParam name
-- -}
