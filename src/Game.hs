module Game
  ( Game (Game)
  , makeGame
  , putGame
  , putMaybeGame
  , boardForState
  , applyMove
  , playMove
  , parseMove
  , availableMoves
  , availableMovesForPos
  ) where

import Position
import Pieces
import Lib

import Data.Map as Map
import Text.Printf as TP
import Data.List as DL
import Data.Maybe as DM

import Data.Char

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

data TurnState = WaitingForMove | ContinueMove Pos deriving (Eq, Show)

data Game = Game { boardState :: GameState
                 , activePlayer :: Player
                 , turnState :: TurnState
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
makeGame = Game initialPositions WhitePlayer WaitingForMove (PosConstraint 1 10 1 10)

boardForState :: GameState -> String
boardForState stateMap =
  let grid = finiteCoordGrid 10
      displayFun = (\(x, y) -> case (Map.lookup (Pos x y) stateMap) of
                       Just p -> ((show p) ++ " ")
                       Nothing -> ((show $ emptyColor (x, y))) ++ " ")
      internalGrid = mapOverGrid displayFun grid
      topsyTurvy = reverse $ transpose $ internalGrid
      topsyLines = DL.map concat topsyTurvy
      yLabels = reverse $ [ (TP.printf "%2d" (x :: Int)) ++ "| " | x <- [1..10]]
      topsyYLabels = zipWith (++) yLabels topsyLines
      topsyXSprtrs = "  |____________________"
      topsyXLabels = "    A B C D E F G H I J"
  in unlines $ ["         "] ++ topsyYLabels ++ [topsyXSprtrs, topsyXLabels]


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

data Move = MoveSimple Pos BasePosShift deriving (Show, Eq)

playMove :: Maybe Move -> Game -> Either (Game, String)  Game
playMove Nothing g = Left (g, "No move provided.")
playMove (Just m) g = case applyMove m g of
                 Left s -> Left (g, s)
                 Right newGameState -> Right newGameState

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
-- validY (ord '0') = Just 10
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


-- look there harder
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Monad.html#v:liftM
applyMove :: Move -> Game -> Either String Game
applyMove (MoveSimple pos s) g =
  case Map.lookup pos (boardState g) of
    Nothing -> Left $ "No piece at position " ++ (show pos) ++ "."

    Just (Piece Pawn p) ->
      if (p == (activePlayer g))
      then
        let actPlr = activePlayer g
            bsz = boardSize g
            bst = boardState g
        in
          do
            { case shift bsz s pos of
                Nothing -> Left $ "can't move outside the board"
                Just closestPos -> case Map.lookup closestPos bst of
                  -- we're moving onto empty square
                  Nothing -> gameAfterMove g pos closestPos []
                  Just (Piece _ plr) -> if plr == actPlr
                    -- we're moving our piece into other our piece
                    then Left $ "There's your piece there"
                    -- we're next to opponent, need to check if one after is empty
                    else do
                    { case shift bsz s closestPos of
                        Nothing -> Left $ "Opponent at the edge of the board."
                        Just posBehindOpp -> case Map.lookup posBehindOpp bst of
                          -- position behind opponent piece is empty
                          Nothing -> gameAfterMove g pos posBehindOpp [closestPos]
                        -- position behind opponent piece is occupied
                        _ -> Left $ "Position behind opponent blocked."
                  }
            }

      else Left $ "That's not your piece."

    Just (Piece King _) -> undefined

gameAfterMove :: Game -> Pos -> Pos -> [Pos] -> Either String Game
gameAfterMove g posBegin posEnd removals =
  case Map.lookup posBegin (boardState g) of
    Nothing -> Left $ "There's no piece here"
    Just piece ->
      let newState = Map.insert posEnd piece (Map.delete posBegin (boardState g))
          canContinue = ((length removals) > 0) && ((length $ availableMoves g) > 0)
      in if canContinue
      then Right (gameAdvanceState
                  g
                  (removePieces newState removals)
                  (activePlayer g)
                  (ContinueMove posEnd))
      else Right (gameAdvanceState
                  g
                  (removePieces newState removals)
                  (opponentOf $ activePlayer g)
                  (WaitingForMove))

removePieces :: GameState -> [Pos] -> GameState
removePieces gs [] = gs
removePieces gs (x:xs) = removePieces (Map.delete x gs) xs

gameAdvanceState :: Game -> GameState -> Player -> TurnState -> Game
gameAdvanceState g gs plr turnSt = Game gs plr turnSt (boardSize g)

availableMoves :: Game -> [Move]
availableMoves (Game boardSt activePlr WaitingForMove boardSz) =
  undefined
availableMoves g@(Game _ _ (ContinueMove p) _) =
  availableMovesForPos g p

availableMovesForPos :: Game -> Pos -> [Move]
availableMovesForPos g p =
  DL.filter (isValidMove g) ([(MoveSimple p dir) | dir <- [NE,NW,SE,SW]])

isValidMove :: Game -> Move -> Bool
isValidMove g m =
  case turnState g of
    WaitingForMove -> isValidMoveInitTurn g m
    ContinueMove _ -> isValidMoveContinuation g m


isValidMoveInitTurn :: Game -> Move -> Bool
isValidMoveInitTurn g (MoveSimple p dir) =
  case Map.lookup p (boardState g) of
    Nothing -> False
    Just piece -> let bsz = boardSize g
                      targetNear = shift bsz dir p
                      targetFar = shiftM bsz dir targetNear
                  in isValidMoveImpl (boardState g) piece targetNear targetFar

isValidMoveImpl :: GameState -> Piece -> Maybe Pos -> Maybe Pos -> Bool
isValidMoveImpl _ _ Nothing _ = False
isValidMoveImpl gs _ (Just pos) Nothing = Map.notMember pos gs
isValidMoveImpl gs p (Just pos1) (Just pos2) = case Map.lookup pos1 gs of
  Nothing -> True
  Just otherPiece -> if (owner p) == (owner otherPiece)
                     then False
                     else Map.notMember pos2 gs

isValidMoveContinuation :: Game -> Move -> Bool
isValidMoveContinuation g (MoveSimple pos dir) =
  let (ContinueMove p) = turnState g
  in if p /= pos
     then False
     else let bsz = boardSize g
              bst = boardState g
              targetNear = shift bsz dir pos
              targetFar = shiftM bsz dir targetNear
          in isValidContinuation
             bst
             (DM.fromJust (Map.lookup pos bst))
             targetNear
             targetFar
  

isValidContinuation :: GameState -> Piece -> Maybe Pos -> Maybe Pos -> Bool
isValidContinuation _ _ Nothing _ = False
isValidContinuation _ _ _ Nothing = False
isValidContinuation gs p (Just pos1) (Just pos2) =
  case Map.lookup pos1 gs of
    Nothing -> False
    Just otherPiece -> if (owner p) == (owner otherPiece)
                       then False
                       else Map.notMember pos2 gs
  


