module Pieces
  ( Player (WhitePlayer, BlackPlayer)
  , PieceKind (Pawn, King)
  , Piece (Piece)
  ) where

import Position

data Player = WhitePlayer | BlackPlayer deriving (Eq, Show)
data PieceKind = Pawn | King deriving (Eq, Show)

data Piece = Piece PieceKind Player deriving (Eq)
instance Show Piece where
  show (Piece Pawn WhitePlayer) = "w"
  show (Piece Pawn BlackPlayer) = "b"
  show (Piece King WhitePlayer) = "W"
  show (Piece King BlackPlayer) = "B"


-- replace key in map
-- case M.lookup k0 myMap of
--    Nothing -> myMap
--    Just e  -> M.insert k1 e (M.delete k0 myMap)
