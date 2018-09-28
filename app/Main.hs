module Main where

import           Game
--import           Game.Move

import           System.IO
--import           System.Console.Readline

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  --playGame (Right makeGame)

playGame :: Either (Game, String) Game -> IO ()
playGame (Left (g, s)) = do
  print g
  putStrLn $ "ERROR!" ++ s
  playTurn g

playGame (Right g) = do
  print g
  playTurn g

playTurn :: Game -> IO ()
playTurn = undefined
-- playTurn g = do
--   input <- readline "Your Move> "
--   case input of
--     Nothing      -> playGame (Left (g, "Invalid input"))
--     Just "quit"  -> return ()
--     Just "exit"  -> return ()
--     Just moveStr -> playGame (playMove (parseMove moveStr) g)
