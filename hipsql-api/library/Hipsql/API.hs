{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
module Hipsql.API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Servant.API (type (:>), Get, JSON, OctetStream, Post, ReqBody, Summary)
import Servant.API.Generic (type (:-), ToServantApi)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
import qualified Data.Version
import qualified Paths_hipsql_api

type HipsqlAPI = ToServantApi HipsqlRoutes

data HipsqlRoutes route = HipsqlRoutes
  { getVersion :: route :-
      Summary "Gets the current server version"
        :> "version"
        :> Get '[JSON] Version

  , eval :: route :-
      Summary "Evaluate a psql expression"
        :> "eval"
        :> ReqBody '[OctetStream] Lazy.ByteString
        :> Post '[OctetStream] Lazy.ByteString
  } deriving stock (Generic)

theHipsqlAPI :: Proxy HipsqlAPI
theHipsqlAPI = Proxy

theHipsqlApiVersion :: Version
theHipsqlApiVersion = Version $ Data.Version.versionBranch Paths_hipsql_api.version

newtype Version = Version
  { version :: [Int]
  } deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

renderVersion :: Version -> String
renderVersion (Version xs) = List.intercalate "." $ map show xs

isCompatibleWith :: Version -> Version -> Bool
isCompatibleWith (Version xs) (Version ys) =
  take 2 xs == take 2 ys
