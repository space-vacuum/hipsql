{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Async (race, wait, withAsync)
import Data.Foldable (for_)
import Hipsql.Server.Internal (helpMessage)
import Spec.Infra (TestResources(..), startClient, table, test, test', withTimeout, xtable)
import Test.Hspec (describe, hspec, shouldReturn)
import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = hspec do
  let prompt     = "psql> "
  let contPrompt = "      "

  describe "hipsql" do
    describe "client" do
      test' "hipsqlClient" \TestResources {..} -> do
        flip shouldReturn (Right ()) do
          race (startClient port clientIO) do
            readStdout `shouldReturn` prompt
            writeStdin "select 1;"
            readStdout `shouldReturn` table ["?column?"] [["1"]]

            readStdout `shouldReturn` prompt
            writeStdin "select * from ("
            readStdout `shouldReturn` contPrompt
            writeStdin "  values ('a', 'b', 3), ('c', 'd', 4)"
            readStdout `shouldReturn` contPrompt
            writeStdin ") as x(q, w, e)"
            readStdout `shouldReturn` contPrompt
            writeStdin ";"
            readStdout `shouldReturn`
              table ["q", "w", "e"]
                [ ["a", "b", "3"]
                , ["c", "d", "4"]
                ]

    describe "commands" do
      describe "extended display" do
        test "\\x" \TestResources {..} -> do
          readStdout `shouldReturn` prompt
          writeStdin "\\x"
          readStdout `shouldReturn` "Extended display is on."

          readStdout `shouldReturn` prompt
          writeStdin "select * from ("
          readStdout `shouldReturn` contPrompt
          writeStdin "  values ('a', 'b', 3), ('c', 'd', 4)"
          readStdout `shouldReturn` contPrompt
          writeStdin ") as x(q, w, e)"
          readStdout `shouldReturn` contPrompt
          writeStdin ";"
          readStdout `shouldReturn`
            xtable ["q", "w", "e"]
              [ ["a", "b", "3"]
              , ["c", "d", "4"]
              ]

      describe "quit" do
        for_ ["quit", "exit", "\\q"] \quitCommand -> do
          test' quitCommand \TestResources {..} -> do
            withAsync (startClient port clientIO) \handle -> do
              readStdout `shouldReturn` prompt
              writeStdin "select 1;"
              readStdout `shouldReturn` table ["?column?"] [["1"]]
              readStdout `shouldReturn` prompt
              writeStdin quitCommand
              withTimeout "wait handle" (wait handle) `shouldReturn` ()

      describe "help" do
        test "\\?" \TestResources {..} -> do
          readStdout `shouldReturn` prompt
          writeStdin "\\?"
          readStdout `shouldReturn` Char8.unpack helpMessage

      describe "invalid command" do
        test "with empty query buffer" \TestResources {..} -> do
          readStdout `shouldReturn` prompt
          writeStdin "\\foo"
          readStdout `shouldReturn` "invalid command \\foo\nTry \\? for help."

        test "inside of a query" \TestResources {..} -> do
          readStdout `shouldReturn` prompt
          writeStdin "select 1"
          readStdout `shouldReturn` contPrompt
          writeStdin "\\foo"
          readStdout `shouldReturn` "invalid command \\foo\nTry \\? for help."

    describe "errors" do
      test "column does not exist" \TestResources {..} -> do
        readStdout `shouldReturn` prompt
        writeStdin "select foo;"
        readStdout `shouldReturn`
          unlines
            [ "ERROR:  column \"foo\" does not exist"
            , "LINE 1: select foo;"
            , "               ^"
            ]

      test "syntax error" \TestResources {..} -> do
        readStdout `shouldReturn` prompt
        writeStdin "selectfoo;"
        readStdout `shouldReturn`
          unlines
            [ "ERROR:  syntax error at or near \"selectfoo\""
            , "LINE 1: selectfoo;"
            , "        ^"
            ]
