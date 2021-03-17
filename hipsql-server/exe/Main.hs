{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Hipsql.Server (hipsqlWith)
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
main :: IO ()
main = hipsqlWith withLibPQConnect
  where
  withLibPQConnect :: (LibPQ.Connection -> IO ()) -> IO ()
  withLibPQConnect = bracket (LibPQ.connectdb "") LibPQ.finish

