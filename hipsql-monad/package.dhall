let defs = ../defaults.dhall

in      defs.pkg
    //  { name = "hipsql-monad"
        , dependencies =
            defs.pkg.dependencies # [ "postgresql-libpq >= 0.9.4.2 && < 0.10" ]
        }
