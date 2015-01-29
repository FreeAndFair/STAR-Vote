[TODO]: # (Everything that says "not implemented yet" (case insensitive).)

# How to run an election

In the summaries below, prefix `#` means "run as root"; `$` means "run as
user"; and `B` means "visit this location in a browser".

## Building and installing

Summary:

    # apt-get install ghc cabal-install llvm libsqlite3-dev
    $ git clone ssh://git@github.com/GaloisInc/e2eviv
    $ cd e2eviv/src
    $ cabal update
    $ cabal sandbox init
    $ cabal install -j ./append-only-bb ./star-{controller,crypto,keygen,terminal,types,util,voter-db}

1.  Install GHC, cabal-install, llvm 3.4, and sqlite. Often this is as
    simple as `apt-get install ghc cabal-install llvm libsqlite3-dev`
    or your distro's package manager's equivalent. This should be done
    as root. You will need GHC 7.8 or later and cabal-install 1.18 or
    later. If your distro doesn't have a new enough GHC, you can visit
    [the GHC downloads page for 7.8.3](https://www.haskell.org/ghc/download_ghc_7_8_3). If
    your distro doesn't have a new enough cabal-install, you can
    bootstrap an old cabal-install by running `cabal update && cabal
    install cabal-install`, which will put an executable in
    `~/.cabal/bin` by default.
2.  Clone the repository. You've probably already done this, since you're
    looking at this file. If not, github has good instructions on how to do this
    kind of thing; you want the `GaloisInc/e2eviv` repository. If you have
    everything set up correctly, the following will work:

        git clone ssh://git@github.com/GaloisInc/e2eviv
3.  Jump into the elections directory with `cd e2eviv/src`.
4.  Make sure you have a recent package listing with `cabal update`.
5.  For sanity, I recommend creating a sandbox, though this step is not strictly
    necessary. Use `cabal sandbox init`.
6.  Kick off the build. There are several projects here; building them all at
    once will give the dependency solver a better chance of getting things right
    the first time.

        cabal install -j ./append-only-bb ./star-{controller,crypto,terminal,types,util,voter-db}

    This will put executables in `.cabal-sandbox/bin` by default; you can do all the
    usual Unixy things to make them easy to run. The next section will assume
    the executables have found their way onto your path somehow.

## Configuring the election

Summary:

    $ bbserver -b :: -p 8000
    B localhost:8000/reset
    $ star-keygen -b :: -p 8001
    B localhost:8001/register.html
    B localhost:8001/initialize.html
    $ nano star-terminal/start.sh
    $ star-voter-db -b :: -p 8002
    $ curl -X POST localhost:8002/initialize \
        -d voterids='[(3,Voter {_voterName="John Doe", _voterAddress = "Nowhereland"}),(5,Voter {_voterName = "Jane Doe", _voterAddress = "Stix"})]' \
        -d voterstatus='[(3,5,"oregon-2014"),(5,6,"oregon-2014")]'
    $ star-controller -b :: -p 8003
    $ lpoptions -d default_printer
    $ star-terminal/start.sh

1.  Start the append-only bulletin board on port 8000 by running `bbserver`.
    Initialize it by visiting `localhost:8000/reset` and deleting everything.
2.  The election officials should generate an encryption public key, together
    with shares of the private key for each official. Run `star-keygen -p 8001`
    to start a server on port 8001, then visit `localhost:8001/register.html`
    and `localhost:8001/initialize.html` (in that order). The server should
    print a short message on its first running telling where to find the
    configuration information used to contact the bulletin board; the defaults
    should work fine with these instructions.

    The public key should be made available to the voting terminals for
    encryption. Edit the file `star-terminal/start.sh` with your favorite
    editor and modify the `STAR_PUBLIC_KEY` environment variable to contain the
    base64-encoded public key displayed by `initialize.html`.

    The private key shares reported by `initialize.html` should be distributed
    to election officials; these are needed during the vote tallying step
    below. For the purposes of a mock election, one might simply paste them
    into a file for storage until later.

3.  Initialize the voter status database. Run `star-voter-db -b :: -p 8002`
    to start the server on port 8002. You will then need to tell the
    database about all your voters. Below is a sample with some
    synthetic data:

        curl -X POST localhost:8002/initialize \
            -d voterids='[(3,Voter {_voterName="John Doe", _voterAddress = "Nowhereland"}),(5,Voter {_voterName = "Jane Doe", _voterAddress = "Stix"})]' \
            -d voterstatus='[(3,5,"oregon-2014"),(5,6,"oregon-2014")]'

    The `voterids` parameter associates voter IDs with the name and address of
    the voter. The `voterstatus` parameter associates voter IDs with their
    precinct number and the kind of ballot they need to use. The example data
    here puts John Doe in precinct 5 and Jane Doe in precinct 6, both with the
    `oregon-2014` ballot style. Currently `oregon-2014` is the only supported
    ballot style (see `star-util/src/Application/Star/Util.hs`).
4.  Start the controller on port 8003 with `star-controller -b :: -p 8003`.
5.  Configure the default printer for `lp`. You can check the current default
    with `lpstat -p -d` (which will also list the names of non-default
    printers), then select a default by appending the name of the printer you
    wish to make the default to `lpoptions -d`. This default is used by voting
    terminals when it is time to print a ballot and receipt.
6.  Start at least one voting terminal. For a default configuration, run
    `star-terminal/start.sh`, which will start a server on port 8004. You can
    also run `star-terminal/start.sh 8005` and similar to start another
    terminal on port 8005.

## Running the election

StarVote has stations for voter check-in, ballot claims, ballot reading for
completed ballots, vote submission (both casting and spoiling), and an
arbitrary number of extra stations for filling out a ballot. From now on we no
longer assume that the computer where we started all the servers and the
computer at a given station are the same. We will schematically use the domain
`server` below when we need to talk about contacting the computer running all
the servers we set up in the previous step.

*   **Check-in station:** Have a browser open to `server:8002`. Look people up by
    name.
*   **Ballot claim station:** Have a browser open to `server:8003/generateCode`
    and connect a barcode scanner. Keep a terminal open as well for casting and
    spoiling votes (see below).
*   **Voting terminal:** Have a browser open to `server:8004`. Enter a ballot
    code received from the ballot claim station. If you chose to run multiple
    voting terminal servers in the previous step, you can have several stations,
    each with a browser open to a separate server.
*   **Ballot reading:** Not implemented yet! (But can/should be implemented by
    outside parties.)
*   **Vote submission station:** Have a browser open to `server:8003/cast` for
    casting and to `server:8003/spoil` for spoiling. The filled-out ballot
    printed by the voting terminal includes a "Casting ID", visible in plain
    text and encoded as a barcode. The barcode can be scanned to fill out one
    of the two text fields at this station.

## Finalizing the election

Visit `server:8003/tally` and follow the instructions there. The public key and
private shares are the ones that were reported during election startup at
`localhost:8001/initialize.html` (though revisiting this page will generate a
fresh key pair and is thus not a useful thing to do). The public key may be
recovered from `star-terminal/start.sh` if necessary, while the private shares
should be input by election officials. Only a threshold number of private
shares need be entered. The controller will then report the total number of
votes for each race and selection (though any selections which received no
votes at all will be omitted).
