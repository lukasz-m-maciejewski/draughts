module Lib
  ( Location(Location)
  , Grid
  , zipOverGrid
  , zipOverGridWith
  , mapOverGrid
  )
where

import           Text.Printf                   as TP
import           Data.Char                     as DC

type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = (map . map)

data Location = Location {xpos :: Int, ypos :: Int} deriving (Eq)

instance Show Location where
  show = fmtLocAlNum

fmtLocAlNum :: Location -> String
fmtLocAlNum l =
  "(" ++ [xPos2Char $ xpos l] ++ (TP.printf "%02d" $ ypos l) ++ ")"

xPos2Char :: Int -> Char
xPos2Char d = DC.chr ((DC.ord 'A') + (d - 1))
