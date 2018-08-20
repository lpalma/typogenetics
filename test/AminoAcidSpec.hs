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
