Install
========

cabal-install
--------

The whole tutorial is a `cabal <https://cabal.readthedocs.io/en/latest/>`_
project and can be built locally as follows:

.. code-block:: bash

   $ git clone https://github.com/haskell-servant/servant.git
   $ cd servant
   # build
   $ cabal new-build tutorial
   # load in ghci to play with it
   $ cabal new-repl tutorial

stack
--------

The servant `stack <https://docs.haskellstack.org/en/stable/README/>`_ template includes the working tutorial. To initialize this template, run:

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

nix
--------

`Nix <https://nixos.org/nix/>`_ users should feel free to take a look at
the `nix/shell.nix` file in the repository and use it to provision a suitable
environment to build and run the examples.
