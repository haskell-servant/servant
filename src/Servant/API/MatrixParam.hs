{-# LANGUAGE PolyKinds #-}
module Servant.API.MatrixParam where

-- | Lookup the value associated to the @sym@ matrix string parameter
-- and try to extract it as a value of type @a@.
--
-- Example:
--
-- > -- /books;author=<author name>
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get [Book]
data MatrixParam sym a

-- | Lookup the values associated to the @sym@ matrix string parameter
-- and try to extract it as a value of type @[a]@. This is typically
-- meant to support matrix string parameters of the form
-- @param[]=val1;param[]=val2@ and so on. Note that servant doesn't actually
-- require the @[]@s and will fetch the values just fine with
-- @param=val1;param=val2@, too.
--
-- Example:
--
-- > -- /books;authors[]=<author1>;authors[]=<author2>;...
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get [Book]
data MatrixParams sym a

-- | Lookup a potentially value-less matrix string parameter
-- with boolean semantics. If the param @sym@ is there without any value,
-- or if it's there with value "true" or "1", it's interpreted as 'True'.
-- Otherwise, it's interpreted as 'False'.
--
-- Example:
--
-- > -- /books;published
-- > type MyApi = "books" :> MatrixFlag "published" :> Get [Book]
data MatrixFlag sym
