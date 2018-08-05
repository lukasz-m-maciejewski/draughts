module Main where

import Game

import System.IO
import System.Console.Readline

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  playGame (Right makeGame)

playGame :: Either (Game, String) Game -> IO ()
playGame (Left (g, s)) = do
  putStrLn $ show g
  putStrLn $ "ERROR!" ++ s
  playTurn g

playGame (Right g) = do
  putStrLn $ show g
  playTurn g

playTurn :: Game -> IO ()
playTurn g = do
  input <- readline "Your Move> "
  case input of
    Nothing -> playGame (Left (g, "Invalid input"))
    Just "quit" -> return ()
    Just "exit" -> return ()
    Just moveStr -> playGame (playMove (parseMove moveStr) g)
