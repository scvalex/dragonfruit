GHC := ghc -Wall -Werror

.PHONY: all run build dist install clean doc

all: build

run: build
	dist/build/dragonfruit/dragonfruit

build: dist/setup-config
	cabal build

dist: test
	cabal sdist

install: build
	cabal install

clean:
	cabal clean

dist/setup-config: dragonfruit.cabal
	cabal configure

doc: build
	cabal haddock
