Tutorial
========

This is an introductory tutorial to **servant**. Whilst browsing is fine, it makes more sense if you read the sections in order, or at least read the first section before anything else.

Any comments, issues or feedback about the tutorial can be submitted
to `servant's issue tracker <http://github.com/haskell-servant/servant/issues>`_.

Cabal
--------

The whole tutorial is a `cabal <https://cabal.readthedocs.io/en/latest/>`_
project and can be built and played with locally as follows:

.. code-block:: bash

   $ git clone https://github.com/haskell-servant/servant.git
   $ cd servant
   # build
   $ cabal new-build tutorial
   # load in ghci to play with it
   $ cabal new-repl tutorial

Stack
--------

To build the tutorial using `stack <https://docs.haskellstack.org/en/stable/README/>`_ you can run the following:

.. code-block:: bash

   $ stack new myproj servant
   $ cd myproj
   # build
   $ stack build
   # start server
   $ stack exec myproj-exe

The code can be found in the `*.lhs` files under `doc/tutorial/` in the
repository. Feel free to edit it while you're reading this documentation and
see the effect of your changes.

`Nix <https://nixos.org/nix/>`_ users should feel free to take a look at
the `nix/shell.nix` file in the repository and use it to provision a suitable
environment to build and run the examples.

.. toctree::
  :maxdepth: 1

  ApiType.lhs
  Server.lhs
  Client.lhs
  Javascript.lhs
  Docs.lhs
  Authentication.lhs
