{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module BB.DB where

import BB.JSON
import BB.Protocol

import Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.HashDescr
import Crypto.Random
import Crypto.Types.PubKey.ECC

import qualified Data.Aeson as Aeson

import Data.Byteable (toBytes)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 as UTF8

import Data.Convertible

import Data.List (intersperse)

import Data.Monoid

import qualified Data.Text as T

import Data.Time

import Database.HDBC
import Database.HDBC.Sqlite3

import System.Locale (defaultTimeLocale)


-- | Specific concrete representation of authors
data Author = Author { authorName :: T.Text
                     , authorPubKey :: ECDSA.PublicKey
                     }
  deriving (Eq, Show)


-- | Specific concrete representation of messages
data Message = Message { messageText :: T.Text}
  deriving (Eq, Ord)

-- | Specific initial hash to use - corresponding to "0" in the paper
initialHash :: Hash
initialHash = "0"

-- Necessary to store Messages in the DB
instance Convertible SqlValue Message where
  safeConvert = fmap Message . safeConvert

instance Convertible Message SqlValue where
  safeConvert = safeConvert . messageText


-- Represent public keys in the DB using their JSON serialization, just to make things easy
instance Convertible SqlValue ECDSA.PublicKey where
  safeConvert x = decode =<< safeConvert x
    where decode y =
            case Aeson.decode y of
              Just y -> return y
              Nothing -> Left ConvertError { convSourceValue = show x
                                           , convSourceType = "SqlValue"
                                           , convDestType = "ECDSA.PublicKey"
                                           , convErrorMessage = "Couldn't deserialize JSON"
                                           }

instance Convertible ECDSA.PublicKey SqlValue where
  safeConvert = safeConvert . Aeson.encode

-- Signatures are also represented with JSON
instance Convertible SqlValue ECDSA.Signature where
  safeConvert x = decode =<< safeConvert x
    where decode y =
            case Aeson.decode y of
              Just y -> return y
              Nothing -> Left ConvertError { convSourceValue = show x
                                           , convSourceType = "SqlValue"
                                           , convDestType = "ECDSA.PublicKey"
                                           , convErrorMessage = "Couldn't deserialize JSON"
                                           }

instance Convertible ECDSA.Signature SqlValue where
  safeConvert = safeConvert . Aeson.encode


-- The specific posts and history that can be stored in the DB
type DBPost = Post Message Author
type DBHistory = History Message Author


-- | Create the underlying DB tables and keypair
initialize :: (CPRG g) => g -> Connection -> IO ((), g)
initialize g conn = do runRaw conn "DROP TABLE IF EXISTS authors;"
                       runRaw conn "DROP TABLE IF EXISTS board;"
                       runRaw conn "DROP TABLE IF EXISTS config;"
                       runRaw conn mkAuthors
                       runRaw conn mkBoard
                       runRaw conn mkConfig
                       let ((public, private), g') = genKeypair g
                       quickQuery conn "INSERT INTO config (id, publickey, privatekey) VALUES (?, ?, ?);"
                                  [toSql (1::Int), toSql (show public), toSql (show private)]
                       commit conn
                       return ((), g')

  where mkBoard = "CREATE TABLE board (" ++
                  cols [ "id INTEGER PRIMARY KEY ASC"
                       , "message TEXT NOT NULL"
                       , "author_timestamp INTEGER NOT NULL"
                       , "board_timestamp INTEGER NOT NULL"
                       , "author INTEGER NOT NULL"
                       , "hash TEXT NOT NULL"
                       , "author_sig TEXT NOT NULL"
                       , "board_sig TEXT NOT NULL"
                       , "FOREIGN KEY (author) REFERENCES authors(id)"
                       ] ++
                  ");"
        mkAuthors = "CREATE TABLE authors (" ++
                    cols [ "id INTEGER PRIMARY KEY ASC"
                         , "publickey TEXT NOT NULL"
                         , "name TEXT NOT NULL"
                         ] ++
                    ");"
        mkConfig = "CREATE TABLE config (" ++
                   cols [ "id INTEGER NOT NULL UNIQUE CHECK(id=1) DEFAULT 1"
                        , "publickey TEXT NOT NULL"
                        , "privatekey TEXT NOT NULL"
                        ] ++
                   ");"
        cols = concat . intersperse ", "


genKeypair :: (CPRG g) => g -> ((ECDSA.PublicKey, ECDSA.PrivateKey), g)
genKeypair g = let c = getCurveByName SEC_p112r1
               in generate g c

getKeypair :: Connection -> IO (ECDSA.PublicKey, ECDSA.PrivateKey)
getKeypair conn = do [[pub, priv]] <- quickQuery conn "SELECT publickey, privatekey FROM config LIMIT 1;" []
                     let (public, private) = (fromSql pub, fromSql priv)
                     return (read public, read private)

withSqlite3 :: FilePath -> (Connection -> IO a) -> IO a
withSqlite3 db action = do conn <- connectSqlite3 db
                           x <- action conn
                           disconnect conn
                           return x

getAuthors :: Connection -> IO [(Integer, Author)]
getAuthors conn = do authors <- quickQuery conn "SELECT id, name, publickey FROM authors;" []
                     return . map mkAuthor $ authors
  where
    mkAuthor :: [SqlValue] -> (Integer, Author)
    mkAuthor [i, n, k] = (fromSql i, Author (fromSql n) (fromSql k))
    mkAuthor _         = error "SQL garbage author"


getCurrentHash :: forall g. (CPRG g) => g -> Connection -> IO (CurrentHash, g)
getCurrentHash g conn = do mostRecent <- quickQuery conn q []
                           priv <- fmap snd $ getKeypair conn
                           now <- getCurrentTime
                           return $ mkCurrentHash priv now mostRecent

  where q :: String
        q = "SELECT hash FROM board ORDER BY board_timestamp DESC LIMIT 1"

        mkCurrentHash :: ECDSA.PrivateKey -> UTCTime -> [[SqlValue]] -> (CurrentHash, g)
        mkCurrentHash k t []    = mapFst CurrentHash $ sign g k (initialHash, t)
        mkCurrentHash k t [[h]] = mapFst CurrentHash $ sign g k (fromSql h, t)
        mkCurrentHash k t _     = error "invalid DB garbage getting current hash for step 1"

        mapFst f (x, y) = (f x, y)

getHistory :: Connection -> IO DBHistory
getHistory conn = do posts <- quickQuery conn q []
                     return . map mkPost $ posts
  where
    q :: String
    q = "SELECT " ++
        (concat . intersperse ", ") [ "message"
                                    , "author_timestamp"
                                    , "board_timestamp"
                                    , "authors.publickey"
                                    , "authors.name"
                                    , "hash"
                                    , "author_sig"
                                    , "board_sig"
                                    ] ++
        " FROM board, authors " ++
        "WHERE author=authors.id ORDER BY (board.id);"

    mkPost :: [SqlValue] -> DBPost
    mkPost [msg, authorTimestamp, boardTimestamp, authorKey, authorName, hash, authorSig, boardSig] =
      Post { postMessage = fromSql msg
           , postWrittenTime = fromSql authorTimestamp
           , postWriter = Author (fromSql authorName) (fromSql authorKey)
           , postBoardSig = Signed (Signed (fromSql hash) (fromSql authorSig), (fromSql boardTimestamp)) (fromSql boardSig) -- Signed (Signed Hash, UTCTime)
           }
    mkPost _ = error "SQL communication gave garbage"


addAuthor :: Connection
          -> Author
          -> IO ()
addAuthor conn (Author { authorName = name, authorPubKey = key }) =
  do run conn "INSERT INTO authors (id, publickey, name) VALUES(NULL, ?, ?);" -- NULL means auto in SQLite
         [toSql key, toSql name]
     commit conn

