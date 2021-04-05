-- | Provides functions required to start a @hipsql@ server which can be
-- connected to from a client. In many cases it may be more convenient to
-- use the 'Hipsql.Server.Adapter' module.
module Hipsql.Server
  ( startHipsql
  , startHipsql'
  , startHipsqlWith
  , startHipsqlWith'
  , Config(..)
  , getDefaultConfig
  , Deps(..)
  , getDefaultDeps
  ) where

import Hipsql.Server.Internal
