module Utility where

import Text.Parsec

ifNoError :: Either a b -> Maybe b
ifNoError = either (const Nothing) return

inSequence :: Parsec String st a -> Parsec String st b -> Parsec String st (a, b)
inSequence p1 p2 = do
  o1 <- try p1
  o2 <- p2
  return (o1, o2)
