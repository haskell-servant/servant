# With common maintenance tasks

HC ?= ghc-8.4.4

all :
	@echo "Don't try to make all at once!"

really-all :
	 $(MAKE) build-ghc
	 $(MAKE) build-ghc HC=ghc-8.0.2
	 $(MAKE) build-ghc HC=ghc-8.2.2
	 $(MAKE) build-ghc HC=ghc-8.6.5
	 $(MAKE) build-ghcjs

build-ghc :
	cabal v2-build -w $(HC) all

build-ghcjs :
	cabal v2-build --builddir=dist-newstyle-ghcjs --project-file=cabal.ghcjs.project all

packdeps :
	packdeps */*.cabal

doctest : doctest-servant doctest-servant-server
	perl -i -e 'while (<ARGV>) { print unless /package-id\s+base-compat-\d+(\.\d+)*/; }' .ghc.environment.*

doctest-servant :
	(cd servant && doctest src)
	(cd servant && doctest test/Servant/LinksSpec.hs)

doctest-servant-server :
	(cd servant-server && doctest src)
