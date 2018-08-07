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
  , movesFor
  , movesToGames
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

data Move = MoveSimple Pos BasePosShift deriving (Show, Eq, Ord)

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

-- alternative - generate map move -> game for all possible moves
-- this may create overhead but should eliminate duplication
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
          gameIfContinue = (gameAdvanceState
                            g
                            (removePieces newState removals)
                            (ContinueMove posEnd))
          canContinue = ((length removals) > 0)
                        && ((length $ availableContinuationsForPos gameIfContinue posEnd) > 0)
      in if canContinue
      then Right gameIfContinue
      else Right $ gameEndTurn gameIfContinue

removePieces :: GameState -> [Pos] -> GameState
removePieces gs [] = gs
removePieces gs (x:xs) = removePieces (Map.delete x gs) xs

gameAdvanceState :: Game -> GameState -> TurnState -> Game
gameAdvanceState g gs turnSt = Game gs (activePlayer g) turnSt (boardSize g)

gameEndTurn :: Game -> Game
gameEndTurn g = Game (boardState g) (opponentOf $ activePlayer g) WaitingForMove (boardSize g)

availableMoves :: Game -> [Move]
availableMoves g@(Game boardSt activePlr WaitingForMove _) =
  let activePlrPieces = Map.filter (\pc -> ((owner pc) == activePlr)) boardSt
      activePlrPositions = Map.keys activePlrPieces
  in concat $ DL.map (availableMovesForPos g) activePlrPositions

availableMoves g@(Game _ _ (ContinueMove p) _) =
  availableMovesForPos g p

availableMovesForPos :: Game -> Pos -> [Move]
availableMovesForPos g p =
  DL.filter (isValidMove g) ([(MoveSimple p dir) | dir <- [NE,NW,SE,SW]])

availableContinuationsForPos :: Game -> Pos -> [Move]
availableContinuationsForPos g p =
  DL.filter (isValidMoveContinuation g) ([(MoveSimple p dir) | dir <- [NE,NW,SE,SW]])

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
                  in if isForwardDirFor (owner piece) dir 
                     then isValidMoveImpl (boardState g) piece targetNear targetFar
                     else False

isValidMoveImpl :: GameState -> Piece -> Maybe Pos -> Maybe Pos -> Bool
isValidMoveImpl _ _ Nothing _ = False
isValidMoveImpl gs _ (Just pos) Nothing = Map.notMember pos gs
isValidMoveImpl gs p (Just pos1) (Just pos2) = case Map.lookup pos1 gs of
  Nothing -> True
  Just otherPiece -> if (owner p) == (owner otherPiece)
                     then False
                     else Map.notMember pos2 gs

isValidMoveContinuation :: Game -> Move -> Bool
isValidMoveContinuation g mv@(MoveSimple pos _) =
  case turnState g of
    (ContinueMove p) -> if p /= pos
      then False
      else isValidMoveContinuationDecompose g mv
    WaitingForMove -> isValidMoveContinuationDecompose g mv
      
isValidMoveContinuationDecompose :: Game -> Move -> Bool
isValidMoveContinuationDecompose g (MoveSimple pos dir) =
  let bsz = boardSize g
      bst = boardState g
      targetNear = shift bsz dir pos
      targetFar = shiftM bsz dir targetNear
  in isValidContinuationImpl
     bst
     (DM.fromJust (Map.lookup pos bst))
     targetNear
     targetFar

isValidContinuationImpl :: GameState -> Piece -> Maybe Pos -> Maybe Pos -> Bool
isValidContinuationImpl _ _ Nothing _ = False
isValidContinuationImpl _ _ _ Nothing = False
isValidContinuationImpl gs p (Just pos1) (Just pos2) =
  case Map.lookup pos1 gs of
    Nothing -> False
    Just otherPiece -> if (owner p) == (owner otherPiece)
                       then False
                       else Map.notMember pos2 gs

-- refactoring direction
-- 1) genrate all possible moves for individual pieces whether possible or not
-- movesAt :: Game -> [Move]
-- 2) try to applyMoves and generate a map
-- considerMoves :: g -> [Move] -> (Map Move (Either String Game))
-- 3) ask user for input and lookup the move in the map

movesFor :: Game -> [Move]
movesFor (Game boardSt activePlr WaitingForMove _) =
  let activePlrPieces = Map.filter (\pc -> ((owner pc) == activePlr)) boardSt
      activePlrPositions = Map.toList activePlrPieces
  in concat $ DL.map movesForPiece activePlrPositions
movesFor (Game boardSt activePlr (ContinueMove pos) _) =
  case Map.lookup pos boardSt of
    Just piece -> if (owner piece) == activePlr
                  then movesForPiece (pos, piece)
                  else []
    Nothing -> []

movesForPiece :: (Pos, Piece) -> [Move]
movesForPiece (p, (Piece Pawn _)) = ([(MoveSimple p dir) | dir <- [NE,NW,SE,SW]])
movesForPiece (_, (Piece King _)) = undefined

type GameOrErr = Either String Game
type MovesToGames = Map.Map Move GameOrErr

movesToGames :: Game -> [Move] -> MovesToGames
movesToGames g mvs = Map.fromList $ DL.map (\m -> (m, (applyMove m g))) mvs

maybeMakeMove :: Game -> Move -> GameOrErr
maybeMakeMove (Game boardSt plr WaitingForMove boardSz) (MoveSimple pos dir) =
  let posForRegularMove = shift boardSz dir pos
  in case posForRegularMove of
    Nothing -> Left "Can't move outside the board"
    Just nextPos ->
      let pieceAtPos = Map.lookup nextPos boardSt
      in case pieceAtPos of
           Nothing ->
             if not (isForwardDirFor plr dir)
             then Left "pawn can't move backwards"
             else undefined
           Just piece ->
             if (owner piece) == plr
             then Left "player's piece already there"
             else undefined
           
