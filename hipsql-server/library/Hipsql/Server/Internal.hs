{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hipsql.Server.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Hipsql.Server.Internal
  ) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (race_)
import Control.Exception (Exception, SomeException, catch, fromException, throwIO)
import Control.Monad (mfilter)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString(fromString))
import Data.Traversable (for)
import GHC.Stack (SrcLoc, prettySrcLoc)
import Hipsql.API (HipsqlRoutes(HipsqlRoutes, eval, getVersion), HipsqlAPI, theHipsqlAPI, theHipsqlApiVersion)
import Hipsql.API.Internal (lookupHipsqlPort)
import Servant.Server
  ( Handler(Handler), HasServer(ServerT), ServerError(errBody), Application, Server, err400, err500
  , hoistServer, serve
  )
import Servant.Server.Generic (genericServerT)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import qualified Data.List as List
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Network.Wai.Handler.Warp as Warp

data ServerEnv = ServerEnv
  { conn :: LibPQ.Connection
  , killswitch :: MVar ()
  , state :: IORef ServerState
  }

-- | Runtime state of the pseudo psql session.
newtype ServerState = ServerState
  { extendedDisplay :: Bool
  }

newServerEnv :: LibPQ.Connection -> IO ServerEnv
newServerEnv conn = do
  killswitch <- newEmptyMVar
  state <- newIORef ServerState
    { extendedDisplay = False
    }
  pure ServerEnv { conn, killswitch, state }

toHandler :: IO a -> Handler a
toHandler = Handler . ExceptT . handleServantErr
  where
  handleServantErr :: IO a -> IO (Either ServerError a)
  handleServantErr x =
    catch
      (fmap Right x)
      (pure . Left . toServantError)

  toServantError :: SomeException -> ServerError
  toServantError e = case fromException e of
    Just (QueryError m) -> err400 { errBody = Lazy.Char8.fromStrict m }
    Nothing -> err500 { errBody = fromString $ show e }

server :: ServerEnv -> Server HipsqlAPI
server env = hoistServer theHipsqlAPI toHandler ioServer
  where
  ioServer :: ServerT HipsqlAPI IO
  ioServer = genericServerT HipsqlRoutes
    { getVersion = pure theHipsqlApiVersion
    , eval
    }

  eval input = case Lazy.Char8.uncons input of
    Just ('\\', c) -> runCommand c
    _ | input `elem` ["quit", "exit"] -> quit
    _ -> runQuery input

  quit = do
    putMVar (killswitch env) ()
    mempty

  runCommand = \case
    "x" -> runToggleExtendedDisplay
    "?" -> runHelp
    s | s `elem` ["q", "quit"] -> quit
    s -> invalidCommand s

  runHelp = pure $ Lazy.fromStrict helpMessage

  invalidCommand s = do
    pure $ "invalid command \\" <> s <> "\nTry \\? for help."

  runToggleExtendedDisplay = do
    x <- toggleExtendedDisplay
    pure $ "Extended display is " <> (if x then "on" else "off") <> "."

  modify f = do
    atomicModifyIORef' (state env) \s -> let s' = f s in (s', s')

  toggleExtendedDisplay = do
    s <- modify \s -> s { extendedDisplay = not (extendedDisplay s) }
    pure $ extendedDisplay s

  runQuery q = do
    if Lazy.Char8.null q then do
      mempty
    else do
      renderQueryResult env =<< rawQuery env (Lazy.toStrict q)

-- | Render a 'QueryResponse' received from issuing a query as human
-- readable. Respects the current 'extendedDisplay' state.
renderQueryResult :: ServerEnv -> QueryResponse -> IO Lazy.ByteString
renderQueryResult env QueryResponse { columnNames, resultRows } = do
  ServerState { extendedDisplay } <- readIORef (state env)
  let rendered =
        if extendedDisplay then
          renderXTable renderedColNames renderedValues
        else
          renderTable (Just renderedColNames) renderedValues
  pure $ rendered <> "\n" <> renderedRowCount <> "\n"
  where
  renderedColNames = map (fromMaybe "?") columnNames
  renderedValues = map (map (fromMaybe "null")) resultRows
  renderedRowCount = case length resultRows of
    1 -> "(1 row)"
    n -> "(" <> fromString (show n) <> " rows)"

-- | Special case of 'renderTable' which renders the supplied header
-- and values via 'extendedDisplay'.
renderXTable :: [ByteString] -> [[ByteString]] -> Lazy.ByteString
renderXTable hs = Lazy.Char8.intercalate "\n" . zipWith go [1..]
  where
  go :: Int -> [ByteString] -> Lazy.ByteString
  go i rs =
       "-[ RECORD " <> fromString (show i) <> " ]\n"
    <> renderTable Nothing (zipL hs rs)

  zipL :: [a] -> [a] -> [[a]]
  zipL = zipWith \a1 a2 -> [a1, a2]

-- | Render the supplied optional header and values as a table for printing in
-- the console. Attempts to match closely to real psql output.
renderTable :: Maybe [ByteString] -> [[ByteString]] -> Lazy.ByteString
renderTable maybeHeader rows = renderedHeader <> renderedTable
  where
  maxLens =
    map
      (\col -> if null col then 0 else maximum (map Char8.length col))
      (List.transpose (fromMaybe [] maybeHeader : rows))

  -- We don't care about the last column since it never needs to be padded,
  -- so 'safeInit' to omit it.
  maxPadAt :: Int -> Int
  maxPadAt i = maybe 0 (1+) $ listToMaybe $ drop i $ safeInit maxLens

  lineLenAt :: Int -> Int
  lineLenAt i = maybe 0 (2+) $ listToMaybe $ drop i maxLens

  renderCell i s =
    " " <> Lazy.Char8.fromStrict s <>
      Lazy.Char8.replicate
        (fromIntegral (maxPadAt i - Char8.length s))
        ' '

  renderRow = zipWith renderCell [0..]

  renderedHeader = case maybeHeader of
    Nothing -> ""
    Just header ->
      Lazy.Char8.intercalate "|" (renderRow header) <> "\n"
        <> Lazy.Char8.intercalate "+"
            ( map (\i -> Lazy.Char8.replicate (fromIntegral (lineLenAt i)) '-') [0..(length header - 1)]
            ) <> "\n"

  renderedTable =
    Lazy.Char8.intercalate "\n"
      $ map (Lazy.Char8.intercalate "|" . renderRow) rows

-- | Same as 'init' except returns an empty list if the supplied list is empty.
safeInit :: [a] -> [a]
safeInit xs = if null xs then xs else init xs

-- | Execute the supplied query and return its response.
rawQuery :: ServerEnv -> ByteString -> IO QueryResponse
rawQuery env q = do
  liftIO do
    LibPQ.exec (conn env) q >>= \case
      Just result -> do
        maybeMessage <- LibPQ.resultErrorMessage result
        case mfilter (not . Char8.null) maybeMessage of
          Nothing -> mkQueryResponse result
          Just message -> throwIO $ QueryError message
      Nothing -> do
        maybeMessage <- LibPQ.errorMessage (conn env)
        throwIO $ QueryError $ fromMaybe "ERROR: (no message)" maybeMessage

-- | Construct a 'QueryResponse' from a 'LibPQ.Result'.
-- NOTE: This is dangerous as it does not validate that an
-- error did not occur. Prefer 'rawQuery' instead.
mkQueryResponse :: LibPQ.Result -> IO QueryResponse
mkQueryResponse result = do
  numRows <- LibPQ.ntuples result
  numCols <- LibPQ.nfields result
  columnNames <-
    for [0..(numCols - 1)] \c -> do
      LibPQ.fname result c
  resultRows <-
    for [0..(numRows - 1)] \r -> do
      for [0..(numCols - 1)] \c -> do
        LibPQ.getvalue' result r c
  pure QueryResponse
    { columnNames
    , resultRows
    }

-- | An error response from issuing a query.
newtype QueryError = QueryError ByteString
  deriving stock (Show)
  deriving anyclass (Exception)

-- | A successful response from issuing a query.
data QueryResponse = QueryResponse
  { columnNames :: [Maybe ByteString]
  , resultRows :: [[Maybe ByteString]]
  }

helpMessage :: ByteString
helpMessage =
  Char8.intercalate "\n"
    [ "General"
    , "  \\q        quit psql"
    , ""
    , "Formatting"
    , "  \\x        toggle expanded output"
    ]

application :: ServerEnv -> Application
application env = serve theHipsqlAPI (server env)

-- | Same as 'hipsql' but allows you to specify the @port@ directly.
hipsql' :: Maybe SrcLoc -> Config -> LibPQ.Connection -> IO ()
hipsql' loc Config { port, logger } conn = do
  env <- newServerEnv conn
  logger $
    "Starting hipsql server on port "
      <> show port
      <> "; called at "
      <> maybe "<unknown>" prettySrcLoc loc
  race_ (waitForKillswitch env) (Warp.run port (application env))
  where
  waitForKillswitch env = do
    takeMVar (killswitch env)
    hPutStrLn stderr "Shutting down hipsql server"

data Config = Config
  { port :: Int
  , logger :: String -> IO ()
  }

getDefaultConfig :: IO Config
getDefaultConfig = do
  lookupHipsqlPort >>= \case
    Left message -> do
      error $ "Failed to start hipsql server; could not parse port: " <> message
    Right port -> do
      pure Config
        { port
        , logger = hPutStrLn stderr
        }

-- | Start a pseudo psql session with the given 'LibPQ.Connection'.
-- The server port defined by the @HIPSQL_PORT@ environment variable
-- will be used. If unset, the port @9283@ will be used.
hipsql :: Maybe SrcLoc -> LibPQ.Connection -> IO ()
hipsql loc conn = do
  config <- getDefaultConfig
  hipsql' loc config conn

-- | Same as 'hipsql' except uses a 'LibPQ.Connection' acquiring function.
-- Useful when integrating with libraries like @postgresql-simple@ which
-- give you exclusive access to the 'LibPQ.Connection' via such a function.
hipsqlWith :: Maybe SrcLoc -> ((LibPQ.Connection -> IO ()) -> IO ()) -> IO ()
hipsqlWith loc f = f (hipsql loc)

-- | Same as 'hipsqlWith' but allows you to specify the @port@ directly.
hipsqlWith' :: Maybe SrcLoc -> Config -> ((LibPQ.Connection -> IO ()) -> IO ()) -> IO ()
hipsqlWith' loc config f = f (hipsql' loc config)

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
