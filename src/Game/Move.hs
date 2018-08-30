module Game.Move where

import Position
import Data.Char

data Move = MoveSimple Pos BasePosShift deriving (Show, Eq, Ord)

parseMove :: String -> Maybe Move
parseMove s = do
  { x <- validX (s !! 0)
  ; y <- validY (s !! 1)
  ; dir <- validDir (s !! 3) (s !! 4)
  ; Just (MoveSimple (Pos x y) dir)
  }

validX :: Char -> Maybe Int
validX c = if ((ord 'A') <= (ord c)) && ((ord c) <= (ord 'J'))
           then Just ((ord c) - (ord 'A') + 1)
           else Nothing

validY :: Char -> Maybe Int
validY '0' = Just 10
validY c = if ((ord '1') <= (ord c)) && ((ord c) <= (ord '9'))
           then Just ((ord c) - (ord '0'))
           else Nothing

validDir :: Char -> Char -> Maybe BasePosShift
validDir 'N' 'E' = Just NE
validDir 'N' 'W' = Just NW
validDir 'S' 'E' = Just SE
validDir 'S' 'W' = Just SW
validDir _ _ = Nothing
