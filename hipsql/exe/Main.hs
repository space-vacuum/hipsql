module Main where

import Hipsql (startPsqlWith)
import qualified Control.Exception as Exception
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Use env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
main :: IO ()
main = startPsqlWith withConn
  where
  withConn :: (LibPQ.Connection -> IO ()) -> IO ()
  withConn f = Exception.bracket (LibPQ.connectdb mempty) f LibPQ.finish
