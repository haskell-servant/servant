# Example Projects

- **[example-servant-minimal](https://github.com/haskell-servant/example-servant-minimal)**:

	A minimal example for a web server written using **servant-server**,
	including a test-suite using [**hspec**](http://hspec.github.io/) and
	**servant-client**.


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


- **[example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent)**:

  An example for a web server written with **servant-server** and
  [persistent](https://www.stackage.org/package/persistent) for writing data
  into a database.
