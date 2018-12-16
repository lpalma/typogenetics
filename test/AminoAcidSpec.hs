module AminoAcidSpec (spec) where

import AminoAcid
import Strand
import Test.Hspec
import Control.Monad.State

strandA :: Strand
strandA = fromList [A, C, T, G]

strandB :: Strand
strandB = fromList [A, C, G]

spec :: Spec
spec = do
  describe "AminoAcid.del" $ do
    it "does nothing when Strand is empty" $ do
      runState (del Empty) 0 `shouldBe` (Empty, 0)

    it "deletes the currently bound Base and binds to the next" $ do
      runState (del strandA) 2 `shouldBe` (strandB, 2)

    it "does not change Strand when Position is ahead of last Base" $ do
      runState (del strandA) 4 `shouldBe` (strandA, 4)

  describe "AminoAcid.mvr" $ do
    it "does nothing when Strand is empty" $ do
      runState (mvr Empty) 1 `shouldBe` (Empty, 1)

    it "moves one Position to the right without changing Strand" $ do
      runState (mvr strandA) 1 `shouldBe` (strandA, 2)
