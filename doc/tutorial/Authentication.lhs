# Authentication in Servant

Once you've established the basic routes and semantics of your API, it's time
to consider protecting parts of it. Authentication and authorization are broad
and nuanced topics.

As of Servant 0.15, authentication using basicauth, JWT tokens or cookies is
supported via https://github.com/haskell-servant/servant-auth

For authorization see [servant isssue #991](https://github.com/haskell-servant/servant/issues/911).

NOTE: Since Servant 0.15 `Servant.API.BasicAuth` and
`Servant.API.Experimental.Auth` are deprecated.
