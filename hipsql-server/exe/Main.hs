{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Hipsql.Server (hipsqlWith)
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
main :: IO ()
main = go
  where
  go :: (HasCallStack) => IO ()
  go = do
    hipsqlWith
      (fmap snd $ listToMaybe $ getCallStack callStack)
      withLibPQConnect

  withLibPQConnect :: (LibPQ.Connection -> IO ()) -> IO ()
  withLibPQConnect = bracket (LibPQ.connectdb "") LibPQ.finish

