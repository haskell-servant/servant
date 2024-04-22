# servant-quickcheck

`servant-quickcheck` provides tools to test properties across entire APIs.
Rather than writing the same tests for each endpoint, with `servant-quickcheck`
you can simply specify properties every endpoint must meet once and for all.
For example:

``` haskell
followsBestPractices :: Spec
followsBestPractices = describe "my API" $ do

  it "follows best practices" $ do
    withServantServer myAPI myServer $ \burl ->
      serverSatisfies api burl stdArgs
           ( not500
         <%> onlyJsonObjects
         <%> getsHaveCacheControlHeader
         <%> headsHaveCacheControlHeader
         <%> mempty)
```

Additionally, `servant-quickcheck` provides a `serversEqual` function that
generates arbitrary requests (that conform to the description of an API) and
tests that two servers respond identically to them. This can be useful when
refactoring or rewriting an API that should not change.
