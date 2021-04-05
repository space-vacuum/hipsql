# hipsql

|Package|Version|
|-|-|
|hipsql-api|[![Hackage](https://img.shields.io/hackage/v/hipsql-api.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/hipsql-api)|
|hipsql-client|[![Hackage](https://img.shields.io/hackage/v/hipsql-client.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/hipsql-client)|
|hipsql-monad|[![Hackage](https://img.shields.io/hackage/v/hipsql-monad.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/hipsql-monad)|
|hipsql-server|[![Hackage](https://img.shields.io/hackage/v/hipsql-server.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/hipsql-server)|

---

Provides a pseudo `psql` shell that can be invoked using a `LibPQ.Connection` from within a Haskell program.

This is particularly useful when wanting to use something analogous to a _breakpoint_ in the middle of
a test suite to inspect the state of the database during a running transaction.
