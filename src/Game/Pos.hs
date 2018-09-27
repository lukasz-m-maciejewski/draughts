module Game.Pos where

data Pos = Pos { x_ :: Int, y_ :: Int } deriving (Eq, Ord)
instance Show Pos where
  show (Pos x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Direction = NW | NE | SW | SE deriving (Eq, Show, Ord)
data PosShift = PosShift Int Direction

shiftUnconstrained :: Direction -> Pos -> Pos
shiftUnconstrained NW (Pos x y) = Pos (x - 1) (y + 1)
shiftUnconstrained NE (Pos x y) = Pos (x + 1) (y + 1)
shiftUnconstrained SW (Pos x y) = Pos (x - 1) (y - 1)
shiftUnconstrained SE (Pos x y) = Pos (x + 1) (y - 1)

diagonalDist :: Pos -> Pos -> Maybe Int
diagonalDist (Pos x1 y1) (Pos x2 y2) =
  if abs (y2 - y1) == abs (x2 - x1) then Just $ abs (x2 - x1) else Nothing
