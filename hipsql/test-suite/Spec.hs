{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -ddump-minimal-imports -dumpdir /tmp #-}
module Main where

import Control.Concurrent.Async (race, wait, withAsync)
import Data.Foldable (for_)
import Hipsql.Internal (startPsql', startPsqlWith', withLibPQConnect)
import Spec.Infra (TestResources(TestResources, psqlIO, readStdout, writeStdin), table, test, withTimeout, xtable)
import Test.Hspec (describe, hspec, shouldReturn)

main :: IO ()
main = hspec do
  describe "hipsql" do
    describe "launchers" do
      test "startPsql" \TestResources {..} -> do
        withLibPQConnect \conn -> do
          flip shouldReturn (Right ()) do
            race (startPsql' psqlIO conn) do
              readStdout `shouldReturn` "psql> "
              writeStdin "select 1;"
              readStdout `shouldReturn` table ["?column?"] [["1"]]
              readStdout `shouldReturn` "psql> "

      test "startPsqlWith" \TestResources {..} -> do
        flip shouldReturn (Right ()) do
          race (startPsqlWith' psqlIO withLibPQConnect) do
            readStdout `shouldReturn` "psql> "
            writeStdin "select 1;"
            readStdout `shouldReturn` table ["?column?"] [["1"]]

            readStdout `shouldReturn` "psql> "
            writeStdin "select * from ("
            readStdout `shouldReturn` "      "
            writeStdin "  values ('a', 'b', 3), ('c', 'd', 4)"
            readStdout `shouldReturn` "      "
            writeStdin ") as x(q, w, e)"
            readStdout `shouldReturn` "      "
            writeStdin ";"
            readStdout `shouldReturn`
              table ["q", "w", "e"]
                [ ["a", "b", "3"]
                , ["c", "d", "4"]
                ]

    describe "commands" do
      describe "extended display" do
        test "\\x" \TestResources {..} -> do
          flip shouldReturn (Right ()) do
            race (startPsqlWith' psqlIO withLibPQConnect) do
              readStdout `shouldReturn` "psql> "
              writeStdin "\\x"
              readStdout `shouldReturn` "Extended display is on."

              readStdout `shouldReturn` "psql> "
              writeStdin "select * from ("
              readStdout `shouldReturn` "      "
              writeStdin "  values ('a', 'b', 3), ('c', 'd', 4)"
              readStdout `shouldReturn` "      "
              writeStdin ") as x(q, w, e)"
              readStdout `shouldReturn` "      "
              writeStdin ";"
              readStdout `shouldReturn`
                xtable ["q", "w", "e"]
                  [ ["a", "b", "3"]
                  , ["c", "d", "4"]
                  ]

      describe "quit" do
        for_ ["quit", "exit", "\\q"] \quitCommand -> do
          test quitCommand \TestResources {..} -> do
            withAsync (startPsqlWith' psqlIO withLibPQConnect) \handle -> do
              readStdout `shouldReturn` "psql> "
              writeStdin "select 1;"
              readStdout `shouldReturn` table ["?column?"] [["1"]]
              readStdout `shouldReturn` "psql> "
              writeStdin quitCommand
              withTimeout "wait handle" (wait handle) `shouldReturn` ()

    describe "errors" do
      test "column does not exist" \TestResources {..} -> do
        flip shouldReturn (Right ()) do
          race (startPsqlWith' psqlIO withLibPQConnect) do
            readStdout `shouldReturn` "psql> "
            writeStdin "select foo;"
            readStdout `shouldReturn`
              unlines
                [ "ERROR:  column \"foo\" does not exist"
                , "LINE 1: select foo;"
                , "               ^"
                ]

      test "syntax error" \TestResources {..} -> do
        flip shouldReturn (Right ()) do
          race (startPsqlWith' psqlIO withLibPQConnect) do
            readStdout `shouldReturn` "psql> "
            writeStdin "selectfoo;"
            readStdout `shouldReturn`
              unlines
                [ "ERROR:  syntax error at or near \"selectfoo\""
                , "LINE 1: selectfoo;"
                , "        ^"
                ]
