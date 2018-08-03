module Main where

import Game

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  playGame (Right makeGame)

playGame :: Either Game Game -> IO ()
playGame (Left g) = do
  putStrLn $ show g
  putStr "ERROR! Your move> "
  moveStr <- getLine
  putStrLn $ "You chose: " ++ moveStr
  playGame (playMove (parseMove moveStr) g)

playGame (Right g) = do
  putStrLn $ show g
  putStr "Your move> "
  moveStr <- getLine
  putStrLn $ "You chose: " ++ moveStr
  playGame (playMove (parseMove moveStr) g)

