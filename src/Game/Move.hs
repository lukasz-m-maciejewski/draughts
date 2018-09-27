module Game.Move where

import           Game.Pos
import           Data.Char
import           Text.Parsec

import           Utility

data Move = MoveSimple Pos Direction
          | LineMove Pos Pos
          deriving (Show, Eq, Ord)

moveSource :: Move -> Pos
moveSource (LineMove   s _) = s
moveSource (MoveSimple s _) = s

moveTarget :: Move -> Pos
moveTarget (LineMove   _ t) = t
moveTarget (MoveSimple s d) = shiftUnconstrained d s

directionOf :: Move -> Maybe Direction
directionOf (MoveSimple _             d            ) = Just d
directionOf (LineMove   s@(Pos x0 y0) t@(Pos x1 y1)) = do
  d <- diagonalDist s t
  return $ toDirection ((x1 - x0) `div` d) ((y1 - y0) `div` d)
 where
  toDirection :: Int -> Int -> Direction
  toDirection 1    1    = NE
  toDirection (-1) 1    = NW
  toDirection 1    (-1) = SE
  toDirection (-1) (-1) = SW
  toDirection _ _ =
    error "this should be impossible; value should not arise in calculation"

parseMove :: String -> Maybe Move
parseMove = parseMoveSimple validPosElement

parseMoveSimple :: (Int -> Maybe Int) -> String -> Maybe Move
parseMoveSimple posTest s = do
  (p, bs) <- ifNoError (parse moveSimpleParser "" s)
  MoveSimple <$> buildPos posTest p <*> pure bs
 where
  buildPos :: (Int -> Maybe Int) -> (Int, Int) -> Maybe Pos
  buildPos test (x, y) = Pos <$> test x <*> test y

moveSimpleParser :: Parsec String st ((Int, Int), Direction)
moveSimpleParser = do
  _  <- many (oneOf " \t")
  o1 <- try chessPosPreparser
  _  <- many (oneOf " \t")
  o2 <- basePosShiftParser
  _  <- many (oneOf " \t")
  return (o1, o2)

chessPosPreparser :: Parsec String st (Int, Int)
chessPosPreparser = inSequence (digitToX <$> oneOf "ABCDEFGHIJ")
                               positiveNatural
 where
  digitToX :: Char -> Int
  digitToX c = 1 + ord c - ord 'A'
  positiveNatural :: Parsec String st Int
  positiveNatural = foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

basePosShiftParser :: Parsec String st Direction
basePosShiftParser = readBasePosShift <$> inSequence (oneOf "NS") (oneOf "EW")
 where
  readBasePosShift ('N', 'E') = NE
  readBasePosShift ('N', 'W') = NW
  readBasePosShift ('S', 'E') = SE
  readBasePosShift ('S', 'W') = SW
  readBasePosShift a =
    error ("parser should never contain such pattern:" ++ show a)

validPosElement :: Int -> Maybe Int
validPosElement x = if x > 0 && x <= 10 then Just x else Nothing

jumpoverPos :: Move -> Maybe Pos
jumpoverPos (LineMove s t) = do
  d <- diagonalDist s t
  if d == 1 then Nothing else Just (Pos xn yn)
 where
  f a0 a1 = a0 + (abs (a1 - a0) - 1) * signum (a1 - a0)
  xn = f (x_ s) (x_ t)
  yn = f (y_ s) (y_ t)
jumpoverPos _ = Nothing

moveTraceReversed :: Move -> [Pos]
moveTraceReversed m =
  let s   = moveSource m
      t   = moveTarget m
      md  = diagonalDist s t
      dir = Pos (signum (x_ t - x_ s)) (signum (y_ t - y_ s))
      traceElem i = Pos (x_ s + (i * x_ dir)) (y_ s + (i * x_ dir))
  in  case md of
        Nothing -> []
        Just d  -> reverse $ map traceElem [1 .. (d - 1)]
