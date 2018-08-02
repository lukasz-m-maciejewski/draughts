module Game 
  ( Game (Game)
  , makeGame
  , boardForState
  , playMove
  ) where

import Position
import Pieces
import Lib

import Data.Map as Map
import Text.Printf as TP
import Data.List

type GameState = Map.Map Pos Piece

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

initialPositions :: GameState
initialPositions = Map.fromList (initialPositionsWhitePlayer
                                 ++ initialPositionsBlackPlayer)

data Game = Game { boardState :: GameState
                 , activePlayer :: Player
                 , boardSize :: PosConstraint
                 }
instance Show Game where
  show g =
    (boardForState $ boardState g)
    ++ "\nActive: " ++ (show $ activePlayer g)

makeGame :: Game
makeGame = Game initialPositions WhitePlayer (PosConstraint 1 10 1 10)

boardForState :: GameState -> String
boardForState stateMap =
  let grid = finiteCoordGrid 10
      displayFun = (\(x, y) -> case (Map.lookup (Pos x y) stateMap) of
                       Just p -> ((show p) ++ " ")
                       Nothing -> ((show $ emptyColor (x, y))) ++ " ")
      internalGrid = mapOverGrid displayFun grid
      topsyTurvy = reverse $ transpose $ internalGrid
      topsyLines = Data.List.map concat topsyTurvy
      yLabels = reverse $ [ (TP.printf "%2d" (x :: Int)) ++ "| " | x <- [1..10]]
      topsyYLabels = zipWith (++) yLabels topsyLines
      topsyXSprtrs = "  |____________________"
      topsyXLabels = "    A B C D E F G H I J"
  in unlines $ topsyYLabels ++ [topsyXSprtrs, topsyXLabels]


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

data Move = MoveSimple Pos BasePosShift 

applyMove :: Move -> Game -> Maybe Game
applyMove (MoveSimple pos s) g =
  case Map.lookup pos (boardState g) of
    Nothing -> Nothing
    Just (Piece Pawn p) -> if (p == (activePlayer g))
                           then let closestPos = shift s pos (boardSize g)
                                    in undefined
                           else Nothing
    Just (Piece King _) -> undefined

playMove :: Move -> Game -> Either Game Game
playMove m g = case applyMove m g of
                 Nothing -> Left g
                 Just newGameState -> Right newGameState

                 
