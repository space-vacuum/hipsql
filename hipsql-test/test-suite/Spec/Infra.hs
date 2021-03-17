{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Infra where

import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Hipsql.Client.Internal (ClientIO(..), hipsqlClient)
import Hipsql.Server.Internal (application, newServerEnv, renderTable, renderXTable)
import Network.Wai.Handler.Warp (testWithApplication)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec (Spec, expectationFailure, it)
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Network.HTTP.Client as HTTPClient

-- | Shorthand for a rendered table.
table :: [ByteString] -> [[ByteString]] -> String
table hs rs = Lazy.Char8.unpack (renderTable (Just hs) rs) <> renderRowCount rs

-- | Shorthand for a rendered "extended display" table.
xtable :: [ByteString] -> [[ByteString]] -> String
xtable hs rs = Lazy.Char8.unpack (renderXTable hs rs) <> renderRowCount rs

renderRowCount :: [[ByteString]] -> String
renderRowCount rs =
  case length rs of
    1 -> "\n(1 row)\n"
    n -> "\n(" <> show n <> " rows)\n"

-- | A single test case using 'withTestResources'.
test' :: String -> (TestResources -> IO ()) -> Spec
test' name action = do
  it name do
    withTimeout' 5 name do
      withLibPQConnect \conn -> do
        serverEnv <- newServerEnv conn
        testWithApplication (pure (application serverEnv)) \port -> do
          withTestResources port action

-- | A single test case using 'withTestResources'. Spawns
-- a pseudo psql session via 'race' and asserts that the
-- test completes before the psql session does.
test :: String -> (TestResources -> IO ()) -> Spec
test name f = do
  test' name \resources@TestResources { clientIO, port } -> do
    race (startClient port clientIO) (f resources) >>= \case
      Right () -> pure ()
      Left () ->
        expectationFailure
          "hipsql client unexpectedly terminated before test completed"

startClient :: Int -> ClientIO -> IO ()
startClient port clientIO = hipsqlClient clientIO port theHttpManager

theHttpManager :: HTTPClient.Manager
theHttpManager = unsafePerformIO $ HTTPClient.newManager HTTPClient.defaultManagerSettings
{-# NOINLINE theHttpManager #-}

data TestResources = TestResources
  { readStdout :: IO String
  , writeStdin :: String -> IO ()
  , clientIO :: ClientIO
  , port :: Int
  }

withTestResources :: Int -> (TestResources -> IO ()) -> IO ()
withTestResources port body = do
  let mkStream name = do
        chan <- newChan
        let w s = withTimeout ("write" <> name <> " " <> show s) (writeChan chan s)
        let r = withTimeout ("read" <> name) (readChan chan)
        pure (w, r)
  (writeStdout, readStdout) <- mkStream "Stdout"
  (writeStdin, readStdin) <- mkStream "Stdin"
  body TestResources
    { readStdout
    , writeStdin
    , clientIO = ClientIO
        { inputStrLn' = \s -> liftIO $ writeStdout s >> Just <$> readStdin
        , writeLBSLn' = liftIO . writeStdout . Lazy.Char8.unpack
        }
    , port
    }

withTimeout :: String -> IO a -> IO a
withTimeout = withTimeout' 2

withTimeout' :: Int -> String -> IO a -> IO a
withTimeout' seconds name action =
  timeout (seconds * 1000000) action >>= \case
    Just a -> pure a
    Nothing -> do
      expectationFailure
        ("Failed to complete " <> show name
          <> " within " <> show seconds <> " seconds")
      error "can't get here"

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
--
-- e.g. @PGDATABASE=mydb hipsql@
withLibPQConnect :: (LibPQ.Connection -> IO ()) -> IO ()
withLibPQConnect = bracket (LibPQ.connectdb "") LibPQ.finish
