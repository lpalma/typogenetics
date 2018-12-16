module EnzymeSpec (spec) where

import AminoAcid
import Enzyme
import Strand
import Test.Hspec

enzyme :: Enzyme
enzyme = [rpu, inc, cop, mvr, mvl, swi, lpu, int]

enzymeA :: Enzyme
enzymeA = [mvr, mvr, mvr, del]

strandA :: Strand
strandA = fromList [T,A,G,A,T,C,C,A,G,T,C,C,A,C,T,C,G,A]

strandB :: Strand
strandB = fromList [T,A,G,A,T,C,C,A,G,T,C,C,A,C,A,T,C,G,A]

strandC :: Strand
strandC = fromList [A,T,G,C,T,A,A,G]

strandD :: Strand
strandD = fromList [A,T,G,T,A,A,G]

spec :: Spec
spec = do
  describe "Enzyme.act" $ do
    it "does nothing when Strand is Empty" $ do
      act enzyme Empty `shouldBe` Empty

    it "does nothing when Enzyme is empty" $ do
      act [] strandA `shouldBe` strandA

    it "parses DNA Strands" $ do
      act enzymeA strandC `shouldBe` strandD
