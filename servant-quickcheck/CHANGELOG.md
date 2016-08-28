# 0.0.1.1

- Exclude GHC 7.8 base (it wasn't properly supported anyhow).
- More generous bounds for other packages.

# 0.0.1.0

- Better error messages. Error messages now contain failing predicate, failing
response and (except for response predicates), failing requests.
- Significant changes to RequestPredicate and ResponsePredicate types.
