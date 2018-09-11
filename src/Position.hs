module Position where

import Game.Pos

data PosConstraint = PosConstraint {
  xMin :: Int,
  xMax :: Int,
  yMin :: Int,
  yMax :: Int
  } deriving (Eq)
instance Show PosConstraint where
  show pc = "("
            ++ (show $ xMin pc)
            ++ " < x < "
            ++ (show $ xMax pc)
            ++ ", "
            ++ (show $ yMin pc)
            ++ " < y < "
            ++ (show $ yMax pc)
            ++ ")"

isInside :: Pos -> PosConstraint -> Bool
isInside (Pos x y) pc =
  let xOk = (x >= xMin pc) && (x <= xMax pc)
      yOk = (y >= yMin pc) && (y <= yMax pc)
  in xOk && yOk

shift :: PosConstraint -> BasePosShift -> Pos -> Maybe Pos
shift pc ps p =
  let n = shiftUnconstrained ps p
  in if isInside n pc
     then Just n
     else Nothing

shiftM :: PosConstraint -> BasePosShift -> Maybe Pos -> Maybe Pos
shiftM pc ps (Just p) = shift pc ps p
shiftM _  _  Nothing  = Nothing

shiftTrace :: PosConstraint -> PosShift -> Pos ->  [Maybe Pos]
shiftTrace pc (PosShift n bs) p =
  let functList = map (shiftM pc) (take n $ repeat bs)
  in scanr ($) (Just p) functList
