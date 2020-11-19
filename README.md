# hipsql

Provides a pseudo `psql` shell that can be invoked using a `LibPQ.Connection` from within a Haskell program.

This is particularly useful when wanting to use something analogous to a _breakpoint_ in the middle of
a test suite to inspect the state of the database.
