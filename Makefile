# With common maintenance tasks

HC ?= ghc-8.4.4

all :
	@echo "Don't try to make all at once!"

really-all :
	 $(MAKE) build-ghc
	 $(MAKE) build-ghc HC=ghc-8.0.2
	 $(MAKE) build-ghc HC=ghc-8.2.2
	 $(MAKE) build-ghc HC=ghc-8.6.3
	 $(MAKE) build-ghcjs

build-ghc :
	cabal v2-build -w $(HC) all

build-ghcjs :
	cabal v2-build --builddir=dist-newstyle-ghcjs --project-file=cabal.ghcjs.project all

packdeps :
	packdeps */*.cabal
