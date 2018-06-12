servant – A Type-Level Web DSL
==============================

.. image:: https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png

**servant** is a set of Haskell libraries for writing *type-safe* web
applications but also *deriving* clients (in Haskell and other languages) or
generating documentation for them, and more.

This is achieved by taking as input a description of the web API
as a Haskell type. Servant is then able to check that your server-side request
handlers indeed implement your web API faithfully, or to automatically derive
Haskell functions that can hit a web application that implements this API,
generate a Swagger description or code for client functions in some other
languages directly.

If you would like to learn more, click the tutorial link below.

.. toctree::
  :maxdepth: 2

  tutorial/index.rst
  cookbook/index.rst
  examples.md
  links.rst
  principles.rst
