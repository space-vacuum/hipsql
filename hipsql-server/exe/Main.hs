-- | This executable serves as an example for how to start @hipsql@ from
-- your own program, either directly in 'IO' or within your own 'Monad'.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack)
import Hipsql.Monad (MonadHipsqlAdapter(hipsqlAcquireLibPQ))
import Hipsql.Server (startHipsql)
import Hipsql.Server.Adapter (hipsql)
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
main :: IO ()
main = withLibPQConnect \conn -> do
  demoIO conn
  runReaderT demoReaderT conn

withLibPQConnect :: (LibPQ.Connection -> IO ()) -> IO ()
withLibPQConnect = bracket (LibPQ.connectdb "") LibPQ.finish

-- | Example usage of starting hipsql in 'IO'.
demoIO :: LibPQ.Connection -> IO ()
demoIO conn = do
  putStrLn "hipsql-demo-server: demonstrating IO ..."
  startHipsql loc conn
  where
  loc :: (HasCallStack) => Maybe SrcLoc
  loc = fmap snd $ listToMaybe $ getCallStack callStack

-- | Example usage of starting hipsql with a 'Monad' which has
-- an instance for 'MonadHipsqlAdapter'.
demoReaderT :: ReaderT LibPQ.Connection IO ()
demoReaderT = do
  liftIO $ putStrLn "hipsql-demo-server: demonstrating ReaderT ..."
  hipsql

-- | Example instance for 'MonadHipsqlAdapter'. This is not necessary
-- for using @hipsql@; rather, it is simply convenient.
instance MonadHipsqlAdapter (ReaderT LibPQ.Connection IO) where
  hipsqlAcquireLibPQ f = do
    conn <- ask
    liftIO $ f conn
