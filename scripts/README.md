The release process works roughly like this:

``` bash
./scripts/bump-versions.sh <type-of-bump>
git commit
./scripts/upload.hs
git tag <version> && git push --tags
```
