module Game.Move where

import Game.Pos
import Data.Char
import Text.Parsec

import Utility

data Move = MoveSimple Pos BasePosShift deriving (Show, Eq, Ord)

parseMove :: String -> Maybe Move
parseMove = parseMoveSimple validPosElement

parseMoveSimple :: (Int -> Maybe Int) -> String -> Maybe Move
parseMoveSimple posTest s = do
  (p, bs) <- ifNoError (parse moveSimpleParser "" s)
  MoveSimple <$> buildPos posTest p <*> pure bs
  where
    buildPos :: (Int -> Maybe Int) -> (Int, Int) -> Maybe Pos
    buildPos test (x, y) = Pos <$> test x <*> test y


moveSimpleParser :: Parsec String st ((Int, Int),  BasePosShift)
moveSimpleParser = do
  _ <- many (oneOf " \t")
  o1 <- try chessPosPreparser
  _ <- many (oneOf " \t")
  o2 <- basePosShiftParser
  _ <- many (oneOf " \t")
  return (o1, o2)

chessPosPreparser :: Parsec String st (Int, Int)
chessPosPreparser = inSequence (digitToX <$> oneOf "ABCDEFGHIJ") positiveNatural
  where digitToX :: Char -> Int
        digitToX c = 1 + ord c - ord 'A'
        positiveNatural :: Parsec String st Int
        positiveNatural = foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

basePosShiftParser :: Parsec String st BasePosShift
basePosShiftParser = readBasePosShift <$> inSequence (oneOf "NS") (oneOf "EW")
  where readBasePosShift ('N', 'E') = NE
        readBasePosShift ('N', 'W') = NW
        readBasePosShift ('S', 'E') = SE
        readBasePosShift ('S', 'W') = SW
        readBasePosShift a = error ("parser should never contain such pattern:" ++ (show a))

validPosElement :: Int -> Maybe Int
validPosElement x = if x > 0 && x <= 10 then Just x else Nothing
