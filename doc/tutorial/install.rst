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
the `flake.nix` file in the repository and use it to provision a suitable
environment to build and run the examples.

Note for Ubuntu users
--------

Ubuntu's packages for `ghc`, `cabal`, and `stack` are years out of date.
If the instructions above fail for you,
try replacing the Ubuntu packages with up-to-date versions.
First remove the installed versions:

.. code-block:: bash

   # remove the obsolete versions
   $ sudo apt remove ghc haskell-stack cabal-install

Then install fresh versions of the Haskell toolchain
using the `ghcup <https://www.haskell.org/ghcup/install/>`_ installer.

As of February 2022, one easy way to do this is by running a bootstrap script:

.. code-block:: bash

   $ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

The script is interactive and will prompt you for details about what
you want installed and where.  To install manually,
see `the detailed instructions <https://www.haskell.org/ghcup/install/#manual-install>`_.
