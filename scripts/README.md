The release process works roughly like this:

``` bash
./scripts/bump-versions.sh <type-of-bump>
git commit
./scripts/upload.sh <hackage-usr> <hackage-pwd>
git tag <version> && git push --tags
```
