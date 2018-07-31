module PositionSpec
  ( positionSpec
  ) where

import Test.Hspec
import Position



positionSpec :: IO ()
positionSpec = hspec $ do
  describe "shiftUnconstrained should simply increment coords" $ do
    it "NE should increment both coordinates by 1" $ do
      (shiftUnconstrained NE (Pos 1 1)) `shouldBe` (Pos 2 2)
    it "NW should increment y and decrement x" $ do
      (shiftUnconstrained NW (Pos 1 1)) `shouldBe` (Pos 0 2)
    it "SE should increment x and decrement y" $ do
      (shiftUnconstrained SE (Pos 1 1)) `shouldBe` (Pos 2 0)
    it "SW should decrement both coordinates" $ do
      (shiftUnconstrained SW (Pos 1 1)) `shouldBe` (Pos 0 0)

  describe "shift should not exceed constraints" $ do
    let tc = PosConstraint 1 2 1 2
    it "should cut of when going from top right corner anywhere but SW" $
      do { let p = (Pos 2 2)
         ; shift NE p tc `shouldBe` Nothing
         ; shift NW p tc `shouldBe` Nothing
         ; shift SE p tc `shouldBe` Nothing
         ; shift SW p tc `shouldBe` Just (Pos 1 1) }
    it "should cut of when going from top left corner anywhere but SE" $
      do { let p = (Pos 1 2)
         ; shift NE p tc `shouldBe` Nothing
         ; shift NW p tc `shouldBe` Nothing
         ; shift SE p tc `shouldBe` Just (Pos 2 1)
         ; shift SW p tc `shouldBe` Nothing }
    it "should cut of when going from bottom right corner anywhere but NW" $
      do { let p = (Pos 2 1)
         ; shift NE p tc `shouldBe` Nothing
         ; shift NW p tc `shouldBe` Just (Pos 1 2)
         ; shift SE p tc `shouldBe` Nothing
         ; shift SW p tc `shouldBe` Nothing }
    it "should cut of when going from bottom left corner anywhere but NE" $
      do { let p = (Pos 1 1)
         ; shift NE p tc `shouldBe` Just (Pos 2 2)
         ; shift NW p tc `shouldBe` Nothing
         ; shift SE p tc `shouldBe` Nothing
         ; shift SW p tc `shouldBe` Nothing }





