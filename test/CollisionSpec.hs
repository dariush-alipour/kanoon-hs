module CollisionSpec where

import Test.Hspec

import qualified Collision as Collision

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "all" $ do
    it "returns []" $
    -- --..
    -- ..--
      Collision.all (0, 1) (2, 3) `shouldBe` []

    it "returns [Arbl]" $
    -- .--.
    -- ..--
      Collision.all (1, 2) (2, 3) `shouldBe` [Collision.Arbl]

    it "returns [Arbl, Albr, Acbw, Awbc]" $
    -- ..--
    -- ..--
      Collision.all (2, 3) (2, 3) `shouldBe` [Collision.Awbw]

    it "returns [Albr, Acbw]" $
    -- ..---
    -- ..--.
      Collision.all (2, 4) (2, 3) `shouldBe` [Collision.Albr, Collision.Acbw]

    it "returns [Arbl, Acbw]" $
    -- ..---
    -- ...--
      Collision.all (2, 4) (3, 4) `shouldBe` [Collision.Arbl, Collision.Acbw]

    it "returns [Acbw]" $
    -- ..----
    -- ...--.
      Collision.all (2, 5) (3, 4) `shouldBe` [Collision.Acbw]

    it "returns [Awbc]" $
    -- ...--.
    -- ..----
      Collision.all (3, 4) (2, 5) `shouldBe` [Collision.Awbc]

  describe "first" $ do
    it "returns []" $
    -- --..
    -- ..--
      Collision.first (0, 1) (2, 3) `shouldBe` []

    it "returns [Arbl]" $
    -- ..---
    -- ...--
      Collision.first (2, 4) (3, 4) `shouldBe` [Collision.Arbl]

  describe "any" $ do
    it "returns False" $
    -- --..
    -- ..--
      Collision.any (0, 1) (2, 3) `shouldBe` False

    it "returns True" $
    -- ..---
    -- ...--
      Collision.any (2, 4) (3, 4) `shouldBe` True
