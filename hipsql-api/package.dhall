let defs = ../defaults.dhall

in      defs.pkg
    //  { name = "hipsql-api"
        , dependencies =
              defs.pkg.dependencies
            # [ "aeson >= 1.4.7.1 && < 3"
              , "bytestring >= 0.10.10.0 && < 0.12"
              , "servant >= 0.16 && < 0.20"
              ]
        }
