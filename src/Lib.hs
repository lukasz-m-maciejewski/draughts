module Lib
    ( someFunc
    , Piece (None, WhitePawn, BlackPawn, WhiteKing, BlackKing)
    , SquareColor (White, Black)
    , Location (Location)
    , squareColorAt
    , squareColorAtLoc
    , initialContent
    , initialContentLoc
    , Square (Square)
    , Grid
    , finiteCoordGrid
    , locToSquare
    , initialBoardState
    ) where

import Data.List as DL

someFunc :: IO ()
someFunc = putStrLn "someFunc"

initialBoard = [
  " . b . b . b . b . b",
  " b . b . b . b . b .",
  " . b . b . b . b . b",
  " b . b . b . b . b .",
  " . , . , . , . , . ,",
  " , . , . , . , . , .",
  " . w . w . w . w . w",
  " w . w . w . w . w .",
  " . w . w . w . w . w",
  " w . w . w . w . w ."
        ]

data Piece = None | WhitePawn | BlackPawn | WhiteKing | BlackKing
           deriving (Eq, Show)

data SquareColor = White | Black deriving (Eq, Show)
data Location = Location {xpos :: Integer, ypos :: Integer} deriving (Eq, Show)

tupleToLoc :: (Integer, Integer) -> Location
tupleToLoc (x, y) = Location x y

type Grid a = [[a]]

squareColorAt :: (Integer, Integer) -> SquareColor
squareColorAt (x, y) =
  if even (x + y) then White else Black

squareColorAtLoc :: Location -> SquareColor
squareColorAtLoc l = squareColorAt (xpos l, ypos l)

colorSymbol :: SquareColor -> String
colorSymbol White = "."
colorSymbol Black = ","

initialContent :: (Integer, Integer) -> Piece
initialContent (x, y) = case () of _
                                     | even (x + y) -> None
                                     | y < 5 -> BlackPawn
                                     | y > 6 -> WhitePawn
                                     | otherwise -> None

initialContentLoc :: Location -> Piece
initialContentLoc l = initialContent (xpos l, ypos l)
  
data Square = Square {
                piece :: Piece,
                location :: Location
            }
            deriving Eq

instance Show Square where
  show sq = case piece sq of
      None -> colorSymbol $ squareColorAtLoc $ location sq
      WhitePawn -> "w"
      BlackPawn -> "b"
      WhiteKing -> "W"
      BlackKing -> "B"

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = (map . map)

finiteCoordGrid :: Integer -> Grid Location
finiteCoordGrid n =
  let cols = repeat [1..n]
      rows = map repeat [1..n]
      xyGrid = zipOverGrid cols rows -- chess notation has coords reversed
  in mapOverGrid tupleToLoc xyGrid 

locToSquare :: Location -> Square
locToSquare = (\loc -> Square (initialContentLoc loc) loc)

initialBoardState :: Grid Square
initialBoardState =
  mapOverGrid locToSquare (finiteCoordGrid 10)
