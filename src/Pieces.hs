module Pieces
  ( Player (WhitePlayer, BlackPlayer)
  , opponentOf
  , isForwardDirFor
  , PieceKind (Pawn, King)
  , Piece (Piece, kind, owner)
  ) where

import Game.Pos

data Player = WhitePlayer | BlackPlayer deriving (Eq, Show)

opponentOf :: Player -> Player
opponentOf WhitePlayer = BlackPlayer
opponentOf BlackPlayer = WhitePlayer

isForwardDirFor :: Player -> Direction -> Bool
isForwardDirFor WhitePlayer NE = True
isForwardDirFor WhitePlayer NW = True
isForwardDirFor BlackPlayer SE = True
isForwardDirFor BlackPlayer SW = True
isForwardDirFor _ _ = False

data PieceKind = Pawn | King deriving (Eq, Show)

data Piece = Piece { kind :: PieceKind, owner :: Player } deriving (Eq)
instance Show Piece where
  show (Piece Pawn WhitePlayer) = "w"
  show (Piece Pawn BlackPlayer) = "b"
  show (Piece King WhitePlayer) = "W"
  show (Piece King BlackPlayer) = "B"
