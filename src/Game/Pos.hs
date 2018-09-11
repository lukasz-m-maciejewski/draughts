module Game.Pos where

data Pos = Pos { x_ :: Int, y_ :: Int } deriving (Eq, Ord)
instance Show Pos where
  show (Pos x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data BasePosShift = NW | NE | SW | SE deriving (Eq, Show, Ord)
data PosShift = PosShift Int BasePosShift

shiftUnconstrained :: BasePosShift -> Pos -> Pos
shiftUnconstrained NW (Pos x y)  = Pos (x-1) (y+1)
shiftUnconstrained NE (Pos x y) = Pos (x+1) (y+1)
shiftUnconstrained SW (Pos x y) = Pos (x-1) (y-1)
shiftUnconstrained SE (Pos x y) = Pos (x+1) (y-1)
