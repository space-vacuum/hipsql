-- WARNING: Conditional is necessary to deal with "error: non-portable path to
-- file" in OSX.
let defs = ../defaults.dhall

in      defs.pkg
    //  { name = "hipsql-client"
        , dependencies =
              defs.pkg.dependencies
            # [ "directory >= 1.3.6.0 && < 1.4"
              , "filepath >= 1.4.2.1 && < 1.5"
              , "hipsql-api >= 0.0 && < 0.1"
              , "bytestring >= 0.10.10.0 && < 0.11"
              , "haskeline >= 0.7.5.0 && < 0.8"
              , "http-client >= 0.6.4.1 && < 0.7"
              , "http-types >= 0.12.3 && < 0.13"
              , "mtl >= 2.2.2 && < 2.3"
              , "servant-client >= 0.16 && < 0.17"
              , "servant-client-core >= 0.16 && < 0.17"
              ]
        , executables.hipsql
          =
          { main = "Main.hs"
          , source-dirs = "exe"
          , ghc-options = defs.exe-ghc-options
          , dependencies = [ "hipsql-client" ]
          , when =
            { condition = "os(darwin)"
            , ghc-options = [ "-optP-Wno-nonportable-include-path" ]
            }
          }
        }
