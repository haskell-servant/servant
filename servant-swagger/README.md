# servant-swagger

[![Hackage](https://img.shields.io/hackage/v/servant-swagger.svg)](http://hackage.haskell.org/package/servant-swagger)
[![Stackage LTS](http://stackage.org/package/servant-swagger/badge/lts)](http://stackage.org/lts/package/servant-swagger)
[![Stackage Nightly](http://stackage.org/package/servant-swagger/badge/nightly)](http://stackage.org/nightly/package/servant-swagger)

Swagger 2.0 conforming json for [servant](https://github.com/haskell-servant/servant) APIs.

![servant-swagger robot](http://s16.postimg.org/rndz1wbyt/servant.png)

### Motivation

Swagger is a project used to describe and document RESTful APIs.
Unlike Servant it is language-agnostic and thus is quite popular among developers
in different languages. It also exists for a longer time and has more tools to work with.

This package provides means to generate Swagger specification for a Servant API
and also to partially test whether API conforms with its specification.

Generated Swagger specification then can be used for many things such as
- displaying interactive documentation using [Swagger UI](http://swagger.io/swagger-ui/);
- generating clients and servers in many languages using [Swagger Codegen](http://swagger.io/swagger-codegen/);
- and [many others](http://swagger.io/open-source-integrations/).

### Usage

Please refer to [haddock documentation](http://hackage.haskell.org/package/servant/servant-swagger).

Some examples can be found in [`example/` directory](/example).

### Try it out

All generated swagger specifications can be interactively viewed on [Swagger Editor](http://editor.swagger.io/).

Ready-to-use specification can be served as JSON and interactive API documentation
can be displayed using [Swagger UI](https://github.com/swagger-api/swagger-ui).

Many Swagger tools, including server and client code generation for many languages, can be found on
[Swagger's Tools and Integrations page](http://swagger.io/open-source-integrations/).

### Contributing

We are happy to receive bug reports, fixes, documentation enhancements, and other improvements.

Please report bugs via the [github issue tracker](https://github.com/haskell-servant/servant/issues).

