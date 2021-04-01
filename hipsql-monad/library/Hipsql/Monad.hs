-- | This module provides a low-dependency integration point for
-- supporting @hipsql@ interop in your own 'Monad'.
--
-- In practice, you generally want to define an instance for
-- 'MonadHipsqlAdapter' and use the orphan instance provided by
-- importing 'Hipsql.Server.Adapter' from @hipsql-server@. This way,
-- you can optionally depend on @hipsql-server@ only when you need to
-- but can always depend on @hipsql-monad@, the latter of which would
-- not cause you transitively depend on any additional packages.
module Hipsql.Monad where

import GHC.Stack (HasCallStack)
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Type class for starting a @hipsql@ session in a @Monad m@.
--
-- N.B. You generally do not want to define this instance on your own.
-- See the haddock at the top of this module.
class MonadHipsql m where
  hipsql :: (HasCallStack) => m ()

-- | Convenience type class used for deriving instances of 'MonadHipsql'.
--
-- See the haddock at the top of this module for an explanation.
class MonadHipsqlAdapter m where
  hipsqlAcquireLibPQ :: (LibPQ.Connection -> IO a) -> m a
