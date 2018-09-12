module Game where

import Position
import Pieces
import Lib
import Game.Move
import Game.Pos
import Utility

import qualified Data.Map as Map
import qualified Text.Printf as TP
import qualified Data.List as DL
import qualified Data.Maybe as DM

type BoardState = Map.Map Pos Piece

initialPositionsWhitePlayer :: [ (Pos, Piece) ]
initialPositionsWhitePlayer =
  [ (Pos x y, Piece Pawn WhitePlayer)
  | x <- [1..10]
  , y <- [1..4]
  , even (x + y) ]

initialPositionsBlackPlayer :: [ (Pos, Piece) ]
initialPositionsBlackPlayer =
  [ (Pos x y, Piece Pawn BlackPlayer)
  | x <- [1..10]
  , y <- [7..10]
  , even (x + y) ]

initialPositions :: BoardState
initialPositions = Map.fromList (initialPositionsWhitePlayer
                                 ++ initialPositionsBlackPlayer)

data TurnState = WaitingForMove | ContinueMove Pos deriving (Eq, Show)

data Game = Game { boardState :: BoardState
                 , activePlayer :: Player
                 , turnState :: TurnState
                 , boardSize :: PosConstraint
                 }
instance Show Game where
  show g =
    boardForState (boardState g)
    ++ "\nActive: "
    ++ show (activePlayer g)

putGame :: Game -> IO ()
putGame = print

putMaybeGame :: Maybe Game -> IO ()
putMaybeGame Nothing = putStrLn "Nothing"
putMaybeGame (Just g) = putGame g

makeGame :: Game
makeGame = Game initialPositions WhitePlayer WaitingForMove (PosConstraint 1 10 1 10)

boardForState :: BoardState -> String
boardForState stateMap =
  let grid = finiteCoordGrid 10
      displayFun (x, y) =
        case Map.lookup (Pos x y) stateMap of
          Just p -> show p ++ " "
          Nothing -> show (emptyColor (x, y)) ++ " "
      internalGrid = mapOverGrid displayFun grid
      topsyTurvy = reverse $ DL.transpose internalGrid
      topsyLines = DL.map concat topsyTurvy
      yLabels = reverse [ TP.printf "%2d" (x :: Int) ++ "| " | x <- [1..10]]
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

playMove :: Maybe Move -> Game -> Either (Game, String)  Game
playMove Nothing g = Left (g, "No move provided.")
playMove (Just m) g =
  case Map.lookup m (multiverseFor g) of
    Nothing -> Left (g, "illegal move")
    Just (Left s) -> Left (g, s)
    Just (Right newGameState) -> Right newGameState

type GameOrErr = Either String Game
type MovesToGames = Map.Map Move GameOrErr

multiverseFor :: Game -> MovesToGames -- TODO: filter jumps when jump is available
multiverseFor g = movesToGames g $ movesFor g

movesFor :: Game -> [Move]
movesFor (Game boardSt activePlr WaitingForMove _) =
  let activePlrPieces = Map.filter (\pc -> owner pc == activePlr) boardSt
      activePlrPositions = Map.toList activePlrPieces
  in concatMap movesForPiece activePlrPositions
movesFor (Game boardSt activePlr (ContinueMove pos) _) =
  case Map.lookup pos boardSt of
    Just piece -> if owner piece == activePlr
                  then movesForPiece (pos, piece)
                  else []
    Nothing -> []

movesForPiece :: (Pos, Piece) -> [Move]
movesForPiece (p, Piece Pawn _) = [MoveSimple p dir | dir <- [NE,NW,SE,SW]]
movesForPiece (_, Piece King _) = undefined

movesToGames :: Game -> [Move] -> MovesToGames
movesToGames g mvs = Map.fromList $ DL.map (\m -> (m, maybeMakeMove g m)) mvs

maybeMakeMove :: Game -> Move -> GameOrErr
maybeMakeMove (Game boardSt plr WaitingForMove boardSz) (MoveSimple pos dir) =
  case shift boardSz dir pos of
    Nothing -> Left "Can't move outside the board"
    Just posForMove ->
      let pieceAtPos = Map.lookup posForMove boardSt
      in case pieceAtPos of
        Nothing ->
          if not (isForwardDirFor plr dir)
          then Left "pawn can't move backwards"
          else Right $ Game (unsafeAdvancePiece boardSt pos posForMove)
               (opponentOf plr)
               WaitingForMove
               boardSz
        Just piece ->
          if owner piece == plr
          then Left "player's own piece is in the way"
          else
            case shift boardSz dir posForMove of
              Nothing -> Left "can't jump over the opponent - no board there"
              Just posForJump ->
                case Map.lookup posForJump boardSt of
                  Just _ -> Left "jump blocked by another piece"
                  Nothing ->
                    let maybeEndTurn = Game
                                       (unsafeAdvancePiece
                                        (Map.delete posForMove boardSt)
                                         pos
                                         posForJump)
                                       plr
                                       (ContinueMove posForJump)
                                       boardSz
                    in Right $ endTurnOrWaitForContinuation maybeEndTurn

-- maybeMakeMove :: Game -> Move -> GameOrErr
maybeMakeMove (Game boardSt plr (ContinueMove basePos) boardSz) (MoveSimple pos dir) =
  if basePos /= pos then Left "Cannot move this piece in a contunuation"
  else case shift boardSz dir pos of
    Nothing -> Left "can't move outside the board"
    Just posJumpOver ->
      case Map.lookup posJumpOver boardSt of
        Nothing -> Left "Continuation only available when jumping"
        Just piece ->
          if owner piece == plr
          then Left "Can't jump over own piece"
          else
            case shift boardSz dir posJumpOver of
              Nothing -> Left "Can't jump overboard"
              Just posJumpTo ->
                case Map.lookup posJumpTo boardSt of
                  Just _ -> Left "Jump target blocked"
                  Nothing ->
                    let maybeEndTurn = Game
                                       (unsafeAdvancePiece
                                        (Map.delete posJumpOver boardSt)
                                        pos
                                        posJumpTo)
                                       plr
                                       (ContinueMove posJumpTo)
                                       boardSz
                    in Right $ endTurnOrWaitForContinuation maybeEndTurn
-- maybeMakeMove :: Game -> Move -> GameOrErr
maybeMakeMove g@(Game _ _ WaitingForMove _) m@(LineMove p0 p1) =
  do
    vp0 <- nothingIsAnError (validPos (boardSize g) p0) "invalid start position"
    vp1 <- nothingIsAnError (validPos (boardSize g) p1) "invalid end position"
    piece <- nothingIsAnError (Map.lookup vp0 (boardState g)) "no piece at start position"
             >>= validMoveGeometry vp0 vp1
             >>= validOwner (activePlayer g)
    case kind piece of
      Pawn -> maybeMakePawnMove g m
      King -> maybeMakeKingMove g m
  where
    maybeMakePawnMove :: Game -> Move -> GameOrErr
    maybeMakePawnMove g (LineMove p0' p1') =
      do
        d <- nothingIsAnError (diagonalDist p0' p1') "move must be along a diagonal"
        case d of
          1 -> undefined
          2 -> undefined
          _ -> Left "logic error"
    maybeMakeKingMove :: Game -> Move -> GameOrErr
    maybeMakeKingMove = undefined
    validMoveGeometry :: Pos -> Pos -> Piece -> Either String Piece
    validMoveGeometry p0' p1' p =
      do
        d <- nothingIsAnError (diagonalDist p0' p1') "move must be along a diagonal"
        case kind p of
          Pawn -> if d == 1 || d == 2 then Right p else Left "error"
          King -> Right p
    validOwner :: Player -> Piece -> Either String Piece
    validOwner p pc = if p == owner pc then Right pc else Left "attempting to move opponents piece"

maybeMakeMove _ _ = undefined

unsafeAdvancePiece :: BoardState -> Pos -> Pos -> BoardState
unsafeAdvancePiece bs source target =
  let piece = DM.fromJust $ Map.lookup source bs
  in Map.insert target piece (Map.delete source bs)

endTurnOrWaitForContinuation :: Game -> Game
endTurnOrWaitForContinuation g = if not $ Map.null (Map.filter eitherIsRight (multiverseFor g))
                                 then g
                                 else Game
                                      (boardState g)
                                      (opponentOf $ activePlayer g)
                                      WaitingForMove
                                      (boardSize g)
