{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hipsql.Client.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Hipsql.Client.Internal
  ) where

import Control.Exception (catch, throwIO)
import Control.Monad ((<=<), unless, void)
import Control.Monad.Reader (MonadIO(liftIO), MonadTrans(lift), ReaderT(runReaderT), ask, asks)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Hipsql.API (HipsqlRoutes(eval, getVersion), isCompatibleWith, renderVersion, theHipsqlApiVersion)
import Hipsql.API.Internal (Version, defaultHipsqlPort, lookupHipsqlPort, mkVersion)
import Servant.Client
  ( ClientError(ConnectionError, FailureResponse), ResponseF(responseBody, responseStatusCode)
  , mkClientEnv, parseBaseUrl, runClientM
  )
import Servant.Client.Generic (AsClientT, genericClientHoist)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Types.Status as HTTP
import qualified Paths_hipsql_client
import qualified System.Console.Haskeline as Haskeline
import qualified System.IO as IO

theHipsqlClientVersion :: Version
theHipsqlClientVersion = mkVersion Paths_hipsql_client.version

main :: IO ()
main = run \port -> do
  httpManager <- HTTPClient.newManager HTTPClient.defaultManagerSettings
  hipsqlClient defaultClientIO port httpManager
  where
  run :: (Int -> IO ()) -> IO ()
  run action = do
    getArgs >>= \case
      args | "--help" `elem` args -> do
        putStrLn usage
        exitSuccess

      ["--numeric-version"] -> do
        putStrLn $ renderVersion theHipsqlClientVersion

      ["--api-numeric-version"] -> do
        putStrLn $ renderVersion theHipsqlApiVersion

      ["--version"] -> do
        putStrLn $ "hipsql-client version: " <> renderVersion theHipsqlClientVersion
        putStrLn $ "hipsql-api    version: " <> renderVersion theHipsqlApiVersion

      _ : _ : _ -> do
        abort $ "Invalid arguments\n" <> usage

      [] -> do
        lookupHipsqlPort >>= \case
          Left message -> do
            abort $ "Failed to start hipsql client; could not parse port: " <> message
          Right port -> do
            action port

      [arg] ->
        case readMaybe arg of
          Just port -> action port
          Nothing -> abort $ "Invalid port: " <> show arg <> "\n" <> usage

usage :: String
usage = "Usage: hipsql-client [port=" <> show defaultHipsqlPort <> "]"

abort :: String -> IO a
abort message = do
  IO.hPutStrLn IO.stderr message
  exitFailure

hipsqlClient :: ClientIO -> Int -> HTTPClient.Manager -> IO ()
hipsqlClient io port httpManager = do
  servantClient <- mkServantClient httpManager port
  psqlEnv <- initPsqlEnv' io servantClient
  flip runReaderT psqlEnv $ runInputT defaultSettings psql

-- | Runtime state of the pseudo psql session.
newtype ClientState = ClientState
  { queryBuffer :: ByteString
  }

psql :: PsqlM ()
psql = checkCompatibility *> loop
  where
  checkCompatibility = do
    PsqlEnv { serverApiVersion } <- lift ask
    unless (theHipsqlApiVersion `isCompatibleWith` serverApiVersion) do
      writeLBSLn $
        "WARNING: Client may be incompatible with server: "
          <> "\n  client api version = " <> Lazy.Char8.pack (renderVersion theHipsqlApiVersion)
          <> "\n  server api version = " <> Lazy.Char8.pack (renderVersion serverApiVersion)

  loop = do
    prompt <- getPrompt
    inputStrLn prompt >>= \case
      Nothing -> quit
      Just q -> evalLine q

  defaultPrompt = "hipsql> "

  continuationPrompt = map (const ' ') defaultPrompt

  getPrompt = do
    q <- gets queryBuffer
    pure $ if Char8.null q then defaultPrompt else continuationPrompt

  evalLine s = case s of
    _ | s `elem` ["\\q", "\\quit", "quit", "exit"] -> quit
    '\\' : _ -> runCommand (Lazy.Char8.pack s)
    _ -> runQuery (Char8.pack s)

  runCommand c = do
    writeLBSLn =<< serverEval c
    loop

  runQuery q0 = do
    q <- appendQueryBuffer q0
    if Char8.null q || Char8.last q /= ';' then do
      loop
    else do
      clearQueryBuffer
      writeLBSLn =<< serverEval (Lazy.fromStrict q)
      loop

  quit = do
    void (serverEval "\\q")
      `Haskeline.catch` \case
        -- In case the server shuts down before we're done reading the response.
        ConnectionError _ -> pure ()
        e -> Haskeline.throwIO e

  appendQueryBuffer q = do
    s <- modify \s@ClientState { queryBuffer } ->
          s { queryBuffer =
                if Char8.null queryBuffer then q else queryBuffer <> "\n" <> q
            }
    pure $ queryBuffer s

  clearQueryBuffer = void $ modify \s -> s { queryBuffer = mempty }

data PsqlEnv = PsqlEnv
  { serverApiVersion :: Version
  , state :: IORef ClientState
  , io :: ClientIO
  , serverEval' :: Lazy.ByteString -> IO Lazy.ByteString
  }

data ClientIO = ClientIO
  { inputStrLn' :: String -> PsqlM (Maybe String)
  , writeLBSLn' :: Lazy.ByteString -> PsqlM ()
  }

type PsqlM = InputT (ReaderT PsqlEnv IO)

getServerEval :: ServantClient -> Lazy.ByteString -> IO Lazy.ByteString
getServerEval servantClient input = do
  eval servantClient input `catch` \case
    FailureResponse _ r -> pure $ go r
    e -> throwIO e
  where
  go r = prefix <> message
    where
    prefix = case HTTP.statusCode (responseStatusCode r) of
      400 -> ""
      c -> "HTTP " <> Lazy.Char8.pack (show c) <> ": "

    message =
      if Lazy.Char8.null (responseBody r) then
        "(no message)"
      else
        responseBody r

-- | Access the runtime state with the supplied function.
gets :: (ClientState -> a) -> PsqlM a
gets f = do
  ref <- lift $ asks state
  s <- liftIO $ readIORef ref
  pure $ f s

-- | Modify the runtime state given the supplied function.
modify :: (ClientState -> ClientState) -> PsqlM ClientState
modify f = do
  ref <- lift $ asks state
  liftIO $ atomicModifyIORef' ref \s -> let s' = f s in (s', s')

initPsqlEnv :: ServantClient -> IO PsqlEnv
initPsqlEnv = initPsqlEnv' defaultClientIO

initPsqlEnv' :: ClientIO -> ServantClient -> IO PsqlEnv
initPsqlEnv' io servantClient = do
  serverApiVersion <- getVersion servantClient
  state <- newIORef ClientState { queryBuffer = mempty }
  pure PsqlEnv
    { serverApiVersion
    , state
    , io
    , serverEval' = getServerEval servantClient
    }

defaultClientIO :: ClientIO
defaultClientIO = ClientIO
  { inputStrLn' = getInputLine
  , writeLBSLn' = liftIO . Lazy.Char8.putStrLn
  }

-- | Writes the supplied 'String' to stdout as the shell prompt
-- and reads a line from stdin as a 'String'.
inputStrLn :: String -> PsqlM (Maybe String)
inputStrLn s = do
  f <- lift $ asks $ inputStrLn' . io
  f s

-- | Writes the supplied 'ByteString' to stdout.
writeLBSLn :: Lazy.ByteString -> PsqlM ()
writeLBSLn s = do
  f <- lift $ asks $ writeLBSLn' . io
  f s

serverEval :: Lazy.ByteString -> PsqlM Lazy.ByteString
serverEval s = do
  f <- lift $ asks serverEval'
  liftIO $ f s

type ServantClient = HipsqlRoutes (AsClientT IO)

mkServantClient :: HTTPClient.Manager -> Int -> IO ServantClient
mkServantClient httpManager port = do
  url <- parseBaseUrl $ "127.0.0.1:" <> show port
  let clientEnv = mkClientEnv httpManager url
  pure $
    genericClientHoist $
      either throwIO pure
        <=< flip runClientM clientEnv

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
