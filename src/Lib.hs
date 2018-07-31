module Lib
    ( initialBoard
    , Piece (WhitePawn, BlackPawn, WhiteKing, BlackKing)
    , PlayerColor (WhitePlayer, BlackPlayer)
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
    , Game (Game)
    , fmtGame
    , initialGameState
    , Move (Move)

    , isValidMove
    ) where

import Data.List as DL
import Text.Printf as TP
import Data.Char as DC
import Data.Maybe as DM

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
maxX :: Int
minX :: Int
maxY :: Int
minY :: Int

maxX = 10
minX =  1
maxY = 10
minY =  1


data Piece = WhitePawn | BlackPawn | WhiteKing | BlackKing
           deriving (Eq, Show)

data PlayerColor = WhitePlayer | BlackPlayer deriving (Eq, Show)

owner :: Piece -> PlayerColor
owner WhitePawn = WhitePlayer
owner WhiteKing = WhitePlayer
owner BlackPawn = BlackPlayer
owner BlackKing = BlackPlayer

data SquareColor = White | Black deriving (Eq, Show)
data Location = Location {xpos :: Int, ypos :: Int} deriving (Eq)

fmtLocNum :: Location -> String
fmtLocNum l = "(" ++ (TP.printf "%01d" $ xpos l) ++ ", " ++ (TP.printf "%01d" $ ypos l) ++ ")"

xPos2Char :: Int -> Char
xPos2Char d = DC.chr ((DC.ord 'A') + (d - 1))

fmtLocAlNum :: Location -> String
fmtLocAlNum l = "(" ++ [xPos2Char $ xpos l] ++ (TP.printf "%02d" $ ypos l) ++ ")"

instance Show Location where
  show = fmtLocAlNum

tupleToLoc :: (Int, Int) -> Location
tupleToLoc (x, y) = Location x y

type Grid a = [[a]]

squareColorAt :: (Int, Int) -> SquareColor
squareColorAt (x, y) =
  if odd (x + y) then White else Black

squareColorAtLoc :: Location -> SquareColor
squareColorAtLoc l = squareColorAt (xpos l, ypos l)

colorSymbol :: SquareColor -> String
colorSymbol White = "."
colorSymbol Black = ","

initialContent :: (Int, Int) -> Maybe Piece
initialContent (x, y) = case () of _
                                     | odd (x + y) -> Nothing
                                     | y < 5 -> Just WhitePawn
                                     | y > 6 -> Just BlackPawn
                                     | otherwise -> Nothing

initialContentLoc :: Location -> Maybe Piece
initialContentLoc l = initialContent (xpos l, ypos l)
  
data Square = Square {
                piece :: Maybe Piece,
                location :: Location
            }
            deriving Eq

fmtSquare :: Square -> String
fmtSquare sq = case piece sq of
      Nothing -> colorSymbol $ squareColorAtLoc $ location sq
      Just WhitePawn -> "w"
      Just BlackPawn -> "b"
      Just WhiteKing -> "W"
      Just BlackKing -> "B"

instance Show Square where
  show = fmtSquare 

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = (map . map)

finiteCoordGrid :: Int -> Grid Location
finiteCoordGrid n =
  let cols = repeat [1..n]
      rows = map repeat [1..n]
      xyGrid = zipOverGrid rows cols
  in mapOverGrid tupleToLoc xyGrid 

locToSquare :: Location -> Square
locToSquare = (\loc -> Square (initialContentLoc loc) loc)

initialBoardState :: Grid Square
initialBoardState =
  mapOverGrid locToSquare (finiteCoordGrid 10)

data Game = Game { board :: Grid Square
                 , currentPlayer :: PlayerColor
                 }

fmtGame :: Game -> String
fmtGame g =
  let grid = reverse $ transpose $ board g
      formattedSquares = mapOverGrid
                         (\s -> (fmtSquare s) ++ (show $ location s) ++ " ")
                         grid
      concatThirdLvl = map (foldr (++) []) formattedSquares
  in unlines $ concatThirdLvl
      
initialGameState :: Game
initialGameState = Game initialBoardState WhitePlayer

data Move = Move Location Location [Location]

-- validPawnDestFrom :: Game -> Location -> [Location]
-- validPawnDestFrom p l = 
--   let x = xpos l
--       y = ypos l
--       maybeUpLeft  = if ((&&) (x > minX) (y < maxY))
--         then Just (Location (x - 1) (y + 1))
--         else Nothing
--       maybeUpRight = if ((&&) (x < maxX) (y < maxY))
--         then Just (Location (x + 1) (y + 1))
--         else Nothing
--       maybeDownLeft = if ((&&) (x > minX) (minY < y))
--         then Just (Location (x - 1) (y - 1))
--         else Nothing
--       maybeDownRight = if ((&&) (x < maxX) (minY < y))
--         then Just (Location (x + 1) (y - 1))
--         else Nothing
--   in [maybeUpLeft, maybeUpRight, maybeDownLeft, maybeDownRight]

validMoveHelper :: Location -> Location -> Game -> Bool
validMoveHelper from to g =
  let xshift = (xpos from) - (xpos to)
      yshift = (ypos from) - (ypos to)
      xshiftValid = (xpos to <= maxX) && (xpos to >= minX)
      yshiftValid = (ypos to <= maxY) && (ypos to >= minY)
      moveInsideBoard = xshiftValid && yshiftValid && ((abs xshift) == (abs yshift))
      targetEmpty = not $ locationOccupied to g
      ifJumpByTwoThenNonEmptyInBetween = if (abs xshift) == 1 then True else
        let jumpoverLoc = Location ((xpos from) + (xshift `div` 2)) ((ypos from) + (yshift `div` 2))
            pc = pieceAtLoc jumpoverLoc g
        in False            
  in moveInsideBoard && targetEmpty && ifJumpByTwoThenNonEmptyInBetween
      
  
  

pieceAtLoc :: Location -> Game -> Maybe Piece
pieceAtLoc l g =
  let b = board g
      x = (xpos l) - 1
      y = (ypos l) - 1
      sq = b !! x !! y
  in piece sq

locationOccupied :: Location -> Game -> Bool
locationOccupied l g = case pieceAtLoc l g of
  Nothing -> False
  otherwise -> True
  

isOwner :: Maybe Piece -> PlayerColor -> Bool
isOwner Nothing _ = False
isOwner (Just pc) pl = (owner pc) == pl
  
isValidMove :: Move -> Game -> Bool
isValidMove (Move initLoc targetLoc _) g =
  let player = currentPlayer g
      pieceToMove = pieceAtLoc initLoc g
      possibleMoves = undefined
      correctOwner = isOwner pieceToMove player
      legalTarget = undefined
  in ((&&) correctOwner legalTarget)
