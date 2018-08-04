module Main where

import Game

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  playGame (Right makeGame)

playGame :: Either (Game, String) Game -> IO ()
playGame (Left (g, s)) = do
  putStrLn $ show g
  putStrLn $ "ERROR!" ++ s
  putStr " Your move> "
  moveStr <- getLine
  playGame (playMove (parseMove moveStr) g)

playGame (Right g) = do
  putStrLn $ show g
  putStr "Your move> "
  moveStr <- getLine
  playGame (playMove (parseMove moveStr) g)

