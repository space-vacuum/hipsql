-- | Hipsql interop for @postgresql-tx-simple@.
--
-- N.B. The 'MonadHipsql' instance for 'TxM' is derived by defining
-- a 'MonadHipsqlAdapter' instance and importing the orphan from
-- "Hipsql.Server.Adapter".
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hipsql.Tx.Simple
  ( hipsql
  ) where

import Database.PostgreSQL.Tx (TxEnv, TxM, askTxEnv)
import Database.PostgreSQL.Tx.Unsafe (unsafeRunIOInTxM)
import Hipsql.Monad (MonadHipsql(hipsql), MonadHipsqlAdapter(hipsqlAcquireLibPQ))
import Hipsql.Server.Adapter ()

import qualified Database.PostgreSQL.Simple as Simple
import qualified Database.PostgreSQL.Simple.Internal as Simple.Internal

instance (TxEnv Simple.Connection r) => MonadHipsqlAdapter (TxM r) where
  hipsqlAcquireLibPQ f = do
    conn <- askTxEnv
    unsafeRunIOInTxM $ Simple.Internal.withConnection conn f
