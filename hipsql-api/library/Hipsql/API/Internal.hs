{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Hipsql.API.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Hipsql.API.Internal
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

lookupHipsqlPort :: IO (Either String Int)
lookupHipsqlPort =
  lookupEnvInt "HIPSQL_PORT" `withDefault` Right defaultHipsqlPort

defaultHipsqlPort :: Int
defaultHipsqlPort = 9283

lookupEnvInt :: String -> IO (Maybe (Either String Int))
lookupEnvInt k =
  flip fmap (lookupEnv k) \mv ->
    flip fmap mv \v ->
      case readMaybe v of
        Just i -> Right i
        Nothing -> Left $ "Invalid int for " <> k <> ": " <> v

withDefault :: IO (Maybe a) -> a -> IO a
withDefault action defaultValue = flip fmap action \case
  Nothing -> defaultValue
  Just a -> a

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
