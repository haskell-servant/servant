# With common maintenance tasks

all :
	@echo "Don't try to make all at once!"

build-ghc :
	cabal v2-build all

build-ghcjs :
	cabal v2-build --builddir=dist-newstyle-ghcjs --project-file=cabal.ghcjs.project all

packdeps :
	packdeps */*.cabal
