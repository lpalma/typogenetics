module AminoAcidSpec (spec) where

import AminoAcid
import Strand
import Test.Hspec
import Control.Monad.State

strandA :: Strand
strandA = Cons A (Cons C (Cons T (Cons G Empty)))

strandB :: Strand
strandB = Cons A (Cons C (Cons G Empty))

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

--  describe "AminoAcid.mvl" $ do
--    it "does nothing when Strand is empty" $ do
--      mvl Empty `shouldBe` Empty
--
--    it "moves one position to the left" $ do
--      mvl (BoundStrand [C, T, G] 1) `shouldBe` (BoundStrand [C, T, G] 0)
--
--    it "does nothing when it is not bound to the Strand" $ do
--      mvl (Strand [A, C, A, T]) `shouldBe` Strand [A, C, A, T]
