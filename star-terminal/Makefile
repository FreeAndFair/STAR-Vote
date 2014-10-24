all: build

build: dist/build/star-terminal/star-terminal

dist/build/star-terminal/star-terminal: cabal.sandbox.config src/Main.hs src/Application/StarTerminal/*hs
	cabal build

cabal.sandbox.config:
	cabal sandbox init
	cabal update
	cabal add-source ../star-types
	cabal add-source ../star-util
	cabal install --only-dependencies
