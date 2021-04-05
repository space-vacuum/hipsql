-- | Internal module which implements the @hipsql@ HTTP API using @servant@.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Hipsql.API.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Hipsql.API.Internal
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Version

-- | Lookup the HTTP port to use for a hipsql HTTP server or client
-- by checking the @HIPSQL_PORT@ environment variable. Defaults
-- to 'defaultHipsqlPort' if unset.
lookupHipsqlPort :: IO (Either String Int)
lookupHipsqlPort =
  lookupEnvInt "HIPSQL_PORT" `withDefault` Right defaultHipsqlPort

-- | By default, hipsql should use the port @55805@.
defaultHipsqlPort :: Int
defaultHipsqlPort = 55805

-- | Lookup an environment variable and parse it as an 'Int'.
lookupEnvInt :: String -> IO (Maybe (Either String Int))
lookupEnvInt k =
  flip fmap (lookupEnv k) \mv ->
    flip fmap mv \v ->
      case readMaybe v of
        Just i -> Right i
        Nothing -> Left $ "Invalid int for " <> k <> ": " <> v

-- | Used in conjunction with 'lookupEnvInt' to set a default value.
withDefault :: IO (Maybe a) -> a -> IO a
withDefault action defaultValue = flip fmap action \case
  Nothing -> defaultValue
  Just a -> a

-- | Used to show version information for a hipsql server or client.
newtype Version = Version
  { version :: [Int]
  } deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Constructs a 'Version' from a @Paths_hipsql_*.version@ value.
mkVersion :: Data.Version.Version -> Version
mkVersion = Version . Data.Version.versionBranch

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
