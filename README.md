# servant

[![Build Status](https://secure.travis-ci.org/haskell-servant/servant.svg)](http://travis-ci.org/haskell-servant/servant)

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

These libraries provides a family of combinators to define webservices and automatically generate the documentation and client-side querying functions for each endpoint.

In order to minimize the dependencies depending on your needs, we provide these features under different packages.

- `servant`, which contains everything you need to *declare* a webservice API.
- `servant-server`, which lets you *implement* an HTTP server with handlers for each endpoint of an API.
- `servant-client`, which lets you derive automatically Haskell functions that let you query each endpoint of a `servant` webservice.
- `servant-docs`, which lets you generate API docs for your webservice.
- `servant-jquery`, which lets you derive Javascript functions (based on jquery) to query your API's endpoints, in the same spirit as `servant-client`.

## Getting started

We've written a [Getting Started](http://haskell-servant.github.io/getting-started/) guide that introduces the core types and features of servant. After this article, you should be able to write your first servant webservices, learning the rest from the haddocks' examples.

## Repositories and Haddocks

- The core [servant](http://github.com/haskell-servant) package - [docs](http://hackage.haskell.org/package/servant)
- Implementing an HTTP server for a webservice API with [servant-server](http://github.com/haskell-servant/servant-server) - [docs](http://hackage.haskell.org/package/servant-server)
- (Haskell) client-side function generation with [servant-client](http://github.com/haskell-servant/servant-client) - [docs](http://hackage.haskell.org/package/servant-client)
- (Javascript) client-side function generation with [servant-jquery](http://github.com/haskell-servant/servant-jquery) - [docs](http://hackage.haskell.org/package/servant-jquery)
- API docs generation with [servant-docs](http://github.com/haskell-servant/servant-docs) - [docs](http://hackage.haskell.org/package/servant-docs)
