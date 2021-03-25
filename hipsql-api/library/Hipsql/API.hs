{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
module Hipsql.API where

import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Hipsql.API.Internal (Version(Version), mkVersion)
import Servant.API (type (:>), Get, JSON, OctetStream, Post, ReqBody, Summary)
import Servant.API.Generic (type (:-), ToServantApi)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
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
theHipsqlApiVersion = mkVersion Paths_hipsql_api.version

renderVersion :: Version -> String
renderVersion (Version xs) = List.intercalate "." $ map show xs

isCompatibleWith :: Version -> Version -> Bool
isCompatibleWith (Version xs) (Version ys) =
  take 2 xs == take 2 ys
