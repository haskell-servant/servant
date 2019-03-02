# Example Projects

- **[example-servant-minimal](https://github.com/haskell-servant/example-servant-minimal)**:

    A minimal example for a web server written using **servant-server**,
    including a test-suite using [**hspec**](http://hspec.github.io/) and
    **servant-client**.

- **[servant-examples](https://github.com/sras/servant-examples)**:

    Similar to [the cookbook](https://docs.servant.dev/en/latest/cookbook/index.html) but
    with no explanations, for developers who just want to look at code examples to find out how to do X or Y
    with servant.

- **[stack-templates](https://github.com/commercialhaskell/stack-templates)**

    Repository for templates for haskell projects, including some templates using
    **servant**. These templates can be used with `stack new`.

- **[custom-monad](https://github.com/themoritz/diener)**:

    A custom monad that can replace `IO` in servant applications. It adds among
    other things logging functionality and a reader monad (for database connections).
    A full usage example of servant/diener is also provided.

- **[example-servant-elm](https://github.com/haskell-servant/example-servant-elm)**:

    An example for a project consisting of

    - a backend web server written using **servant-server**,
    - a frontend written in [elm](http://elm-lang.org/) using
      [servant-elm](https://github.com/mattjbray/servant-elm) to generate client
      functions in elm for the API,
    - test-suites for both the backend and the frontend.

- **[servant-purescript](https://github.com/eskimor/servant-purescript/tree/master/examples/central-counter)**:

    An example consisting of

    - a backend that uses `servant`
    - a frontend written in [PureScript](http://www.purescript.org/) using
      [servant-purescript](https://github.com/eskimor/servant-purescript) to generate
      an API wrapper in PureScript to interface the web API with


- **[example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent)**:

    An example for a web server written with **servant-server** and
    [persistent](https://www.stackage.org/package/persistent) for writing data
    into a database.

- **[full-example-servant-elm-auth-yeshql-postgresql](https://github.com/aRkadeFR/FlashCard)**:

    A full open source website written with **servant-server**, yeshql, postgresql and elm 0.19.


- [`import Servant` github search](https://github.com/search?q=%22import+Servant%22+language%3AHaskell&type=Code)

    It has thousands of results and can be a good way to see how people use servant in their projects or even to discover
    servant-related libraries.
