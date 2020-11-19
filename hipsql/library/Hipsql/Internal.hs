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

import Control.Exception (Exception(displayException, fromException), SomeException, throwIO)
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
import System.Console.Haskeline.MonadException (catch)
import qualified Data.ByteString.Char8 as Char8
import qualified Database.PostgreSQL.LibPQ as LibPQ

startPsql :: LibPQ.Connection -> IO ()
startPsql conn = do
  psqlEnv <- mkPsqlEnv conn
  flip runReaderT psqlEnv $ runInputT defaultSettings psql

startPsqlWith :: ((LibPQ.Connection -> IO ()) -> IO ()) -> IO ()
startPsqlWith = ($ startPsql)

type PsqlM = InputT (ReaderT PsqlEnv IO)

data PsqlEnv = PsqlEnv
  { conn :: LibPQ.Connection
  , state :: IORef PsqlState
  }

mkPsqlEnv :: LibPQ.Connection -> IO PsqlEnv
mkPsqlEnv conn = do
  state <- newIORef PsqlState
    { extendedDisplay = False
    , queryBuffer = ""
    }
  pure PsqlEnv { conn, state }

data PsqlState = PsqlState
  { extendedDisplay :: Bool
  , queryBuffer :: String
  }

gets :: (PsqlState -> a) -> PsqlM a
gets f = do
  ref <- lift $ asks state
  s <- liftIO $ readIORef ref
  pure $ f s

modify :: (PsqlState -> PsqlState) -> PsqlM PsqlState
modify f = do
  ref <- lift $ asks state
  liftIO $ atomicModifyIORef' ref \s -> let s' = f s in (s', s')

toggleExtendedDisplay :: PsqlM Bool
toggleExtendedDisplay = do
  s <- modify \s -> s { extendedDisplay = not (extendedDisplay s) }
  pure $ extendedDisplay s

appendQueryBuffer :: String -> PsqlM String
appendQueryBuffer q = do
  s <- modify go
  pure $ queryBuffer s
  where
  go s@PsqlState { queryBuffer } =
    s { queryBuffer = if null queryBuffer then q else queryBuffer <> "\n" <> q }

clearQueryBuffer :: PsqlM ()
clearQueryBuffer = void $ modify \s -> s { queryBuffer = "" }

psql :: PsqlM ()
psql = loop
  where
  loop = do
    prompt <- getPrompt
    getInputLine prompt >>= \case
      Nothing -> pure ()
      Just q -> runCommand q

  getPrompt = do
    q <- gets queryBuffer
    pure $ if null q then "psql> " else "      "

  runCommand = \case
    "exit" -> pure ()
    "quit" -> pure ()
    "\\x" -> runToggleExtendedDisplay
    q -> runQuery q

  runToggleExtendedDisplay = do
    x <- toggleExtendedDisplay
    outputStrLn $ "Extended display is " <> (if x then "on" else "off") <> "."
    loop

  runQuery q0 = do
    q <- appendQueryBuffer q0
    if null q || last q /= ';' then do
      loop
    else do
      clearQueryBuffer
      let try x = fmap Right x `catch` \e -> pure (Left e)
      response <- try $ rawQuery $ fromString q
      res <- renderResponse response
      liftIO $ Char8.putStrLn res
      loop

renderResponse :: Either SomeException QueryResponse -> PsqlM ByteString
renderResponse = \case
  Left e -> do
    case fromException e of
      Just (QueryException msg) -> pure msg
      Nothing -> pure $ "EXCEPTION: " <> Char8.pack (displayException e) <> "\n"
  Right QueryResponse { columnNames, resultRows } -> do
    x <- gets extendedDisplay
    let rendered =
          if x then
            Char8.intercalate "\n" $
              flip map (zip [(1::Int)..] resultRows) \(i, row) ->
                "-[ RECORD " <> Char8.pack (show i) <> " ]\n"
                  <> renderExtended (zip columnNames row)
          else
            renderTable
              (Just (map renderColName columnNames))
              (map (map renderValue) resultRows)
    pure $ rendered <> "\n"
  where
  renderColName = fromMaybe "?"
  renderValue = fromMaybe "null"

  renderExtended row =
    renderTable Nothing (map (\(c, r) -> [renderColName c, renderValue r]) row)

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

safeInit :: [a] -> [a]
safeInit xs = if null xs then xs else init xs

rawQuery :: ByteString -> PsqlM QueryResponse
rawQuery q = do
  c <- lift $ asks conn
  liftIO do
    LibPQ.exec c q >>= \case
      Just result -> do
        maybeMessage <- LibPQ.resultErrorMessage result
        case mfilter (not . Char8.null) maybeMessage of
          Nothing -> mkQueryResponse result
          Just message -> throwIO $ QueryException message
      Nothing -> do
        maybeMessage <- LibPQ.errorMessage c
        throwIO $ QueryException $ fromMaybe "no message" maybeMessage

data QueryResponse = QueryResponse
  { columnNames :: [Maybe ByteString]
  , resultRows :: [[Maybe ByteString]]
  }

data QueryException = QueryException ByteString
  deriving stock (Show)

instance Exception QueryException

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
