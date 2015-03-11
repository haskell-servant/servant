0.3
---
* Add a `Canonicalize` type family that distributes all `:>`s inside `:<|>`s to get to the canonical type of an API -- which is then used in all other packages to avoid weird handler types in *servant-server*.
* Multiple content-type/accept support for all the relevant combinators
* Provide *JSON*, *PlainText*, *OctetStream* and *FormUrlEncoded* content types out of the box
* Type-safe link generation to API endpoints
* Support for the PATCH HTTP method
* Removed the home-made QuasiQuote for writing API types in a more human-friendly format until we come up with a better design for it
* Make most if not all of the haddock code examples run through doctest
* Some general code cleanup