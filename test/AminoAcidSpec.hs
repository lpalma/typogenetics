module AminoAcidSpec (spec) where

import AminoAcid
import Strand
import Test.Hspec

spec :: Spec
spec = do
  describe "AminoAcid.del" $ do
    it "does nothing when Strand is empty" $ do
      del Empty `shouldBe` Empty

    it "deletes the currently bound Base and binds to the next" $ do
      del (BoundStrand [A, C, A, T] 2) `shouldBe` (BoundStrand [A, C, T] 2)

    it "does nothing when it is not bound to the Strand" $ do
      del (Strand [A, C, A, T]) `shouldBe` Strand [A, C, A, T]
  
  describe "AminoAcid.mvr" $ do
    it "does nothing when Strand is empty" $ do
      mvr Empty `shouldBe` Empty

    it "moves one position to the right" $ do
      mvr (BoundStrand [C, T, G] 1) `shouldBe` (BoundStrand [C, T, G] 2)

    it "does nothing when it is not bound to the Strand" $ do
      mvr (Strand [A, C, A, T]) `shouldBe` Strand [A, C, A, T]

  describe "AminoAcid.mvl" $ do
    it "does nothing when Strand is empty" $ do
      mvl Empty `shouldBe` Empty

    it "moves one position to the left" $ do
      mvl (BoundStrand [C, T, G] 1) `shouldBe` (BoundStrand [C, T, G] 0)

    it "does nothing when it is not bound to the Strand" $ do
      mvl (Strand [A, C, A, T]) `shouldBe` Strand [A, C, A, T]
