module Main where

import Hipsql (startPsqlWith)
import Hipsql.Internal (withLibPQConnect)

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
main :: IO ()
main = startPsqlWith withLibPQConnect
