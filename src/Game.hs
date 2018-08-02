module Game
  ( Game (Game)
  , makeGame
  , putGame
  , putMaybeGame
  , boardForState
  , applyMove
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

putGame :: Game -> IO ()
putGame = putStrLn . show

putMaybeGame :: Maybe Game -> IO ()
putMaybeGame Nothing = putStrLn "Nothing"
putMaybeGame (Just g) = putGame g

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

playMove :: Move -> Game -> Either Game Game
playMove m g = case applyMove m g of
                 Nothing -> Left g
                 Just newGameState -> Right newGameState

-- look there harder
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad.html#v:liftM
applyMove :: Move -> Game -> Maybe Game
applyMove (MoveSimple pos s) g =
  case Map.lookup pos (boardState g) of
    Nothing ->
      Nothing

    Just (Piece Pawn p) ->
      if (p == (activePlayer g))
      then
        let actPlr = activePlayer g
            bsz = boardSize g
            bst = boardState g
        in
          do
            { closestPos <- shift bsz s pos
            ; case Map.lookup closestPos bst of
                -- we're moving onto empty square
                Nothing -> gameAfterMove g pos closestPos []
                Just (Piece _ plr) -> if plr == actPlr
                  -- we're moving our piece into other our piece
                  then Nothing
                  -- we're next to opponent, need to check if one after is empty
                  else do
                  { posBehindOpp <- shift bsz s closestPos
                  ; case Map.lookup posBehindOpp bst of
                      -- position behind opponent piece is empty
                      Nothing -> gameAfterMove g pos posBehindOpp [closestPos]
                      -- position behind opponent piece is occupied
                      _ -> Nothing
                  }
            }

      else Nothing

    Just (Piece King _) -> undefined

gameAfterMove :: Game -> Pos -> Pos -> [Pos] -> Maybe Game
gameAfterMove g posBegin posEnd removals =
  case Map.lookup posBegin (boardState g) of
    Nothing -> Nothing
    Just piece ->
      let newState = Map.insert posEnd piece (Map.delete posBegin (boardState g))
      in Just (gameAdvanceState g (removePieces newState removals))

removePieces :: GameState -> [Pos] -> GameState
removePieces g [] = g
removePieces _ _ = undefined

gameAdvanceState :: Game -> GameState -> Game
gameAdvanceState g gs = Game gs (activePlayer g) (boardSize g)
