{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hipsql.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Internals
    module Hipsql.Internal
  ) where

import Control.Monad (mfilter)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (transpose)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString(fromString))
import Data.Traversable (for)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as Char8
import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Start a pseudo psql session with the given 'LibPQ.Connection'.
startPsql :: LibPQ.Connection -> IO ()
startPsql = startPsql' defaultPsqlIO

-- | Start a pseudo psql session with the given 'LibPQ.Connection' acquiring function.
startPsqlWith :: ((LibPQ.Connection -> IO ()) -> IO ()) -> IO ()
startPsqlWith = startPsqlWith' defaultPsqlIO

-- | Same as 'startPsql' but uses the supplied 'PsqlIO' for 'IO' interactions.
startPsql' :: PsqlIO -> LibPQ.Connection -> IO ()
startPsql' io conn = do
  env <- mkPsqlEnv io conn
  flip runReaderT env $ runInputT defaultSettings psql

-- | Same as 'startPsqlWith' but uses the supplied 'PsqlIO' for 'IO' interactions.
startPsqlWith' :: PsqlIO -> ((LibPQ.Connection -> IO ()) -> IO ()) -> IO ()
startPsqlWith' io = ($ startPsql' io)

-- | Uses env vars as described here:
-- https://www.postgresql.org/docs/13/libpq-envars.html
withLibPQConnect :: (LibPQ.Connection -> IO ()) -> IO ()
withLibPQConnect = Exception.bracket (LibPQ.connectdb "") LibPQ.finish

helpMessage :: ByteString
helpMessage =
  Char8.intercalate "\n"
    [ "General"
    , "  \\q        quit psql"
    , ""
    , "Formatting"
    , "  \\x        toggle expanded output"
    ]

-- | The monad used to run the pseudo psql session.
type PsqlM = InputT (ReaderT PsqlEnv IO)

-- | The runtime environment used by the pseudo psql session.
data PsqlEnv = PsqlEnv
  { conn :: LibPQ.Connection
  , state :: IORef PsqlState
  , io :: PsqlIO
  }

-- | 'IO' functionality required by the pseudo psql session. Allows us
-- to DI the use of stdin and stdout for testing.
data PsqlIO = PsqlIO
  { inputStrLn' :: String -> PsqlM (Maybe String)
  , writeStrLn' :: String -> PsqlM ()
  , writeBSLn' :: ByteString -> PsqlM ()
  }

-- | The default implementation of 'PsqlIO' used for real-world pseudo psql sessions.
defaultPsqlIO :: PsqlIO
defaultPsqlIO = PsqlIO
  { inputStrLn' = getInputLine
  , writeStrLn' = outputStrLn
  , writeBSLn' = liftIO . Char8.putStrLn
  }

-- | Writes the supplied 'String' to stdout as the shell prompt
-- and reads a line from stdin as a 'String'.
inputStrLn :: String -> PsqlM (Maybe String)
inputStrLn s = do
  f <- lift $ asks $ inputStrLn' . io
  f s

-- | Writes the supplied 'String' to stdout.
writeStrLn :: String -> PsqlM ()
writeStrLn s = do
  f <- lift $ asks $ writeStrLn' . io
  f s

-- | Writes the supplied 'ByteString' to stdout.
writeBSLn :: ByteString -> PsqlM ()
writeBSLn s = do
  f <- lift $ asks $ writeBSLn' . io
  f s

-- | Constructs a 'PsqlEnv' from the supplied 'PsqlIO' and 'LibPQ.Connection'.
mkPsqlEnv :: PsqlIO -> LibPQ.Connection -> IO PsqlEnv
mkPsqlEnv io conn = do
  state <- newIORef PsqlState
    { extendedDisplay = False
    , queryBuffer = ""
    }
  pure PsqlEnv { conn, state, io }

-- | Runtime state of the pseudo psql session. Stored in 'PsqlEnv'.
data PsqlState = PsqlState
  { extendedDisplay :: Bool
  , queryBuffer :: String
  }

-- | Access the runtime state with the supplied function.
gets :: (PsqlState -> a) -> PsqlM a
gets f = do
  ref <- lift $ asks state
  s <- liftIO $ readIORef ref
  pure $ f s

-- | Modify the runtime state given the supplied function.
modify :: (PsqlState -> PsqlState) -> PsqlM PsqlState
modify f = do
  ref <- lift $ asks state
  liftIO $ atomicModifyIORef' ref \s -> let s' = f s in (s', s')

-- | Toggle the 'extendedDisplay' state, returning its new value.
toggleExtendedDisplay :: PsqlM Bool
toggleExtendedDisplay = do
  s <- modify \s -> s { extendedDisplay = not (extendedDisplay s) }
  pure $ extendedDisplay s

-- | Add a line to the current 'queryBuffer' state.
appendQueryBuffer :: String -> PsqlM String
appendQueryBuffer q = do
  s <- modify go
  pure $ queryBuffer s
  where
  go s@PsqlState { queryBuffer } =
    s { queryBuffer = if null queryBuffer then q else queryBuffer <> "\n" <> q }

-- | Clear the current 'queryBuffer' state.
clearQueryBuffer :: PsqlM ()
clearQueryBuffer = void $ modify \s -> s { queryBuffer = "" }

-- | The pseudo psql interpreter loop.
psql :: PsqlM ()
psql = loop
  where
  loop = do
    prompt <- getPrompt
    inputStrLn prompt >>= \case
      Nothing -> pure ()
      Just q -> eval q

  getPrompt = do
    q <- gets queryBuffer
    pure $ if null q then "psql> " else "      "

  eval = \case
    '\\' : c -> runCommand c
    s | s `elem` ["quit", "exit"] -> quit
    q -> runQuery q

  quit = pure ()

  runCommand = \case
    "x" -> runToggleExtendedDisplay
    "?" -> runHelp
    s | s `elem` ["q", "quit"] -> quit
    s -> invalidCommand s

  runHelp = do
    writeBSLn helpMessage
    loop

  invalidCommand s = do
    writeStrLn $ "invalid command \\" <> s
    writeStrLn "Try \\? for help."
    loop

  runToggleExtendedDisplay = do
    x <- toggleExtendedDisplay
    writeStrLn $ "Extended display is " <> (if x then "on" else "off") <> "."
    loop

  runQuery q0 = do
    q <- appendQueryBuffer q0
    if null q || last q /= ';' then do
      loop
    else do
      clearQueryBuffer
      writeBSLn =<< renderQueryResult =<< rawQuery (fromString q)
      loop

-- | Render a 'QueryResponse' received from issuing a query as human
-- readable. Respects the current 'extendedDisplay' state.
renderQueryResult :: Either QueryError QueryResponse -> PsqlM ByteString
renderQueryResult = \case
  Left (QueryError msg) -> pure msg
  Right QueryResponse { columnNames, resultRows } -> do
    x <- gets extendedDisplay
    let rendered =
          if x then
            renderXTable renderedColNames renderedValues
          else
            renderTable (Just renderedColNames) renderedValues
    pure $ rendered <> "\n"
    where
    renderedColNames = map (fromMaybe "?") columnNames
    renderedValues = map (map (fromMaybe "null")) resultRows

-- | Special case of 'renderTable' which renders the supplied header
-- and values via 'extendedDisplay'.
renderXTable :: [ByteString] -> [[ByteString]] -> ByteString
renderXTable hs = Char8.intercalate "\n" . zipWith go [1..]
  where
  go :: Int -> [ByteString] -> ByteString
  go i rs =
       "-[ RECORD " <> Char8.pack (show i) <> " ]\n"
    <> renderTable Nothing (zipL hs rs)

  zipL :: [a] -> [a] -> [[a]]
  zipL = zipWith \a1 a2 -> [a1, a2]

-- | Render the supplied optional header and values as a table for printing in
-- the console. Attempts to match closely to real psql output.
renderTable :: Maybe [ByteString] -> [[ByteString]] -> ByteString
renderTable maybeHeader rows = renderedHeader <> renderedTable
  where
  maxLens =
    map
      (\col -> if null col then 0 else maximum (map Char8.length col))
      (transpose (fromMaybe [] maybeHeader : rows))

  -- We don't care about the last column since it never needs to be padded,
  -- so 'safeInit' to omit it.
  maxPadAt :: Int -> Int
  maxPadAt i = maybe 0 (1+) $ listToMaybe $ drop i $ safeInit maxLens

  lineLenAt :: Int -> Int
  lineLenAt i = maybe 0 (2+) $ listToMaybe $ drop i maxLens

  renderCell i s = " " <> s <> Char8.replicate (maxPadAt i - Char8.length s) ' '

  renderRow = zipWith renderCell [0..]

  renderedHeader = case maybeHeader of
    Nothing -> ""
    Just header ->
      Char8.intercalate "|" (renderRow header) <> "\n"
        <> Char8.intercalate "+"
            ( map (\i -> Char8.replicate (lineLenAt i) '-') [0..(length header - 1)]
            ) <> "\n"

  renderedTable =
    Char8.intercalate "\n"
      $ map (Char8.intercalate "|" . renderRow) rows

-- | Same as 'init' except returns an empty list if the supplied list is empty.
safeInit :: [a] -> [a]
safeInit xs = if null xs then xs else init xs

-- | Execute the supplied query and return its response.
rawQuery :: ByteString -> PsqlM (Either QueryError QueryResponse)
rawQuery q = do
  c <- lift $ asks conn
  liftIO do
    LibPQ.exec c q >>= \case
      Just result -> do
        maybeMessage <- LibPQ.resultErrorMessage result
        case mfilter (not . Char8.null) maybeMessage of
          Nothing -> Right <$> mkQueryResponse result
          Just message -> pure $ Left $ QueryError message
      Nothing -> do
        maybeMessage <- LibPQ.errorMessage c
        pure $ Left $ QueryError $ fromMaybe "no message" maybeMessage

-- | A successful response from issuing a query.
data QueryResponse = QueryResponse
  { columnNames :: [Maybe ByteString]
  , resultRows :: [[Maybe ByteString]]
  }

-- | An error response from issuing a query.
data QueryError = QueryError ByteString

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

-- $disclaimer
--
-- Changes to this module will not be reflected in the library's version
-- updates.
