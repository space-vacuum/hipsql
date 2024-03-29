let defs = ../defaults.dhall

in      defs.pkg
    //  { name = "hipsql-server"
        , dependencies =
              defs.pkg.dependencies
            # [ "hipsql-api >= 0.1 && < 0.2"
              , "hipsql-monad >= 0.0 && < 0.2"
              , "async >= 2.2.2 && < 2.3"
              , "bytestring >= 0.10.10.0 && < 0.12"
              , "mtl >= 2.2.2 && < 2.3"
              , "postgresql-libpq >= 0.9.4.2 && < 0.10"
              , "servant-server >= 0.16 && < 0.20"
              , "warp >= 3.3.13 && < 3.4"
              ]
        , executables.hipsql-demo-server
          =
          { main = "Main.hs"
          , source-dirs = [ "exe" ]
          , ghc-options = defs.exe-ghc-options
          , dependencies = [ "hipsql-server" ]
          }
        }
