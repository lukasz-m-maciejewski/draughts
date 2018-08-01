module Game 
  ( Game (Game)
  , makeGame
  , fmtGame
  ) where

import Position
import Pieces
import Lib

import Data.Map as Map
type GameState = Map.Map Pos Piece
data Game = Game GameState deriving (Show)

initialPositionsWhitePlayer :: [ (Pos, Piece) ]
initialPositionsWhitePlayer =
  [ ((Pos x y), (Piece Pawn WhitePlayer))
  | x <- [1..10]
  , y <- [1..4]
  , even (x + y) ]

initialPositionsBlackPlayer :: [ (Pos, Piece) ]
initialPositionsBlackPlayer =
  [ ((Pos x y), (Piece Pawn BlackPlayer))
  | x <- [1..10]
  , y <- [7..10]
  , even (x + y) ]

initialPositions :: Map.Map Pos Piece
initialPositions = Map.fromList (initialPositionsWhitePlayer
                                 ++ initialPositionsBlackPlayer)

makeGame :: Game
makeGame = Game initialPositions

finiteCoordGrid :: Int -> Grid (Int, Int)
finiteCoordGrid n =
  let cols = repeat [1..n]
      rows = Prelude.map repeat [1..n]
      xyGrid = zipOverGrid rows cols
  in xyGrid

data SquareColor = WhiteSquare | BlackSquare deriving (Eq)
instance Show SquareColor where
  show WhiteSquare = "."
  show BlackSquare = ","

emptyColor :: (Int, Int) -> SquareColor
emptyColor (x, y) = if odd (x + y) then WhiteSquare else BlackSquare

-- fmtGame :: Game -> String
fmtGame (Game stateMap) =
  let grid = finiteCoordGrid 10
      displayFun = (\(x, y) -> (findWithDefault (emptyColor (x, y))
                                (Pos x y)
                                stateMap))
  in mapOverGrid displayFun grid
