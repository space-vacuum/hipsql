{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-minimal-imports -dumpdir /tmp #-}
module Spec.Infra where

import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Hipsql.Internal (PsqlIO(..), renderTable, renderXTable)
import System.Timeout (timeout)
import Test.Hspec (Spec, expectationFailure, it)
import qualified Data.ByteString.Char8 as Char8 (unpack)

-- | Shorthand for a rendered table.
table :: [ByteString] -> [[ByteString]] -> String
table hs rs = Char8.unpack $ renderTable (Just hs) rs <> "\n"

-- | Shorthand for a rendered "extended display" table.
xtable :: [ByteString] -> [[ByteString]] -> String
xtable hs rs = Char8.unpack $ renderXTable hs rs <> "\n"

-- | A single test case using 'withTestResources'.
test :: String -> (TestResources -> IO ()) -> Spec
test name = it name . withTestResources

data TestResources = TestResources
  { readStdout :: IO String
  , writeStdin :: String -> IO ()
  , psqlIO :: PsqlIO
  }

withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources body = do
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
    , psqlIO = PsqlIO
        { inputStrLn' = \s -> liftIO $ writeStdout s >> Just <$> readStdin
        , writeStrLn' = liftIO . writeStdout
        , writeBSLn' = liftIO . writeStdout . Char8.unpack
        }
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
