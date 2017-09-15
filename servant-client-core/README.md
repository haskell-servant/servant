# servant-client-core

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

HTTP-client-agnostic client functions for servant APIs.

This library should mainly be of interest to backend- and combinator-writers.

## For backend-writers

If you are creating a new backend, you'll need to:

1. Define a `RunClient` instance for your datatype (call it `MyMonad`)
2. Define a `ClientLike` instance. This will look like:

``` haskell
instance ClientLike (MyMonad a) (MyMonad a) where
  mkClient = id
```

3. Re-export the module Servant.Client.Core.Reexport so that your end-users
   can be blissfully unaware of 'servant-client-core', and so each
   backend-package comes closer to the warm hearth of the drop-in-replacement
   equivalence class.

## For combinator-writers

You'll need to define a new `HasClient` instance for your combinator. There are
plenty of examples to guide you in the
[HasClient](src/Servant/Client/Core/Internal/HasClient.hs) module.
