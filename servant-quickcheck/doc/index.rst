servant-quickcheck – QuickCheck entire APIs
============================================

**servant-quickcheck** provides ways of observing and testing the behaviour of
webservers under arbitrary, but sensible, requests. ('Sensible' here means
requests which have the correct type for their arguments (captures, query
params, headers, and request bodies).)

**servant-quickcheck** can currently:

  - Test whether two servers behave identically when provided the same inputs
    in the same order;
  - Test whether certain properties hold true of an entire API (e.g. that an
    API never throws a 500 error);
  - Stress test arbitrary endpoints in an API.

.. toctree::
  :maxdepth: 1

  ServersEqual.lhs
  ServerSatisfies.lhs
  ServerBenchmark.lhs
