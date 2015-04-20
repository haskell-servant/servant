SERVANT_DIR=/tmp/servant-docs-gh-pages

# Make a temporary clone

rm -rf $SERVANT_DIR

git clone . $SERVANT_DIR

cd $SERVANT_DIR

# Make sure to pull the latest

git remote add haskell-servant git@github.com:haskell-servant/servant-docs.git

git fetch haskell-servant

git reset --hard haskell-servant/gh-pages

# Clear everything away

git rm -rf $SERVANT_DIR/*

# Switch back and build the haddocks

cd -

cabal configure --builddir=$SERVANT_DIR

cabal haddock --hoogle --hyperlink-source --html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --builddir=$SERVANT_DIR

commit_hash=$(git rev-parse HEAD)

# Move the HTML docs to the root

cd $SERVANT_DIR

rm *
rm -rf build
mv doc/html/servant-docs/* .
rm -r doc/

# Add everything

git add .

git commit -m "Built from $commit_hash"

# Push to update the pages

git push haskell-servant HEAD:gh-pages

rm -rf $SERVANT_DIR
