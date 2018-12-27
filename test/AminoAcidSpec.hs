module AminoAcidSpec (spec) where

import AminoAcid
import Strand
import Test.Hspec
import Control.Monad.State

strandA :: Strand
strandA = fromList [A, C, T, G]

strandB :: Strand
strandB = fromList [A, C, G]

strandC :: Strand
strandC = fromList [A, C, T, C, G]

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

  describe "AminoAcid.mvl" $ do
    it "does nothing when Strand is Empty" $ do
      runState (mvl Empty) 1 `shouldBe` (Empty, 1)

    it "move one Position to the left without changing the Strand" $ do
      runState (mvl strandA) 2 `shouldBe` (strandA, 1)

    it "does not move to a negative Position" $ do
      runState (mvl strandA) 0 `shouldBe` (strandA, 0)

  describe "AminoAcid.rpu" $ do
    it "does nothing when Strand is Empty" $ do
      runState (rpu Empty) 0 `shouldBe` (Empty, 0)

    it "binds to the next purine to the right" $ do
      runState (rpu strandA) 0 `shouldBe` (strandA, 3)

  describe "AminoAcid.inc" $ do
    it "does nothing when Strand is Empty" $ do
      runState (inc Empty) 0 `shouldBe` (Empty, 0)

    it "inserts C next to current Base and binds to new Base C" $ do
      runState (inc strandA) 2 `shouldBe` (strandC, 3)

  describe "AminoAcid.lpu" $ do
    it "does nothing when Strand is Empty" $ do
      runState (lpu Empty) 2 `shouldBe` (Empty, 2)

    it "binds to the next purine to the left" $ do
      runState (lpu strandA) 2 `shouldBe` (strandA, 0)
