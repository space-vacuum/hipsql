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

lookupHipsqlPort :: IO (Either String Int)
lookupHipsqlPort =
  lookupEnvInt "HIPSQL_PORT" `withDefault` Right defaultHipsqlPort

defaultHipsqlPort :: Int
defaultHipsqlPort = 55805

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

newtype Version = Version
  { version :: [Int]
  } deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

mkVersion :: Data.Version.Version -> Version
mkVersion = Version . Data.Version.versionBranch

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
