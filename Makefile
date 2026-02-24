.PHONY: build clean run

build:
	cabal build

run:
	cabal run

clean:
	rm -rf dist-newstyle
