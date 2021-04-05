-- | Provides an orphan instance of 'MonadHipsql' and re-exports 'hipsql'.
--
-- Given that your 'Monad' has an instance of 'MonadHipsqlAdapter', you should
-- be able to import this module and use its re-exported 'hipsql' function
-- as a breakpoint (of sorts) to drop into a @hipsql@ session.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hipsql.Server.Adapter
  ( hipsql
  ) where

import Data.Maybe (listToMaybe)
import GHC.Stack (callStack, getCallStack)
import Hipsql.Monad (MonadHipsql(hipsql), MonadHipsqlAdapter(hipsqlAcquireLibPQ))
import Hipsql.Server (startHipsql)

instance {-# OVERLAPPABLE #-} (Monad m, MonadHipsqlAdapter m) => MonadHipsql m where
  hipsql = do
    let loc = fmap snd $ listToMaybe $ getCallStack callStack
    hipsqlAcquireLibPQ (startHipsql loc)
