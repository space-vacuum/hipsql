{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Reader (runReaderT)
import Hipsql.Internal (mkPsqlEnv, psql)
import System.Console.Haskeline (defaultSettings, runInputT)
import qualified Database.PostgreSQL.LibPQ as LibPQ

main :: IO ()
main = do
  conn <- LibPQ.connectdb ""
  psqlEnv <- mkPsqlEnv conn
  flip runReaderT psqlEnv $ runInputT defaultSettings psql
