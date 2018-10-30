module EnzymeSpec (spec) where

import AminoAcid
import Enzyme
import Strand
import Test.Hspec

enzyme :: Enzyme
enzyme = [rpu, inc, cop, mvr, mvl, swi, lpu, int]

strandA :: Strand
strandA = BoundStrand [T,A,G,A,T,C,C,A,G,T,C,C,A,C,T,C,G,A] 8

strandB :: Strand
strandB = Strand [T,A,G,A,T,C,C,A,G,T,C,C,A,C,A,T,C,G,A]

strandC :: Strand
strandC = Strand [A,T,G]

spec :: Spec
spec = do
  describe "Enzyme.act" $ do
    it "parses DNA Strands, returning two Strands as product" $ do
      act enzyme strandA `shouldBe` (strandC, strandB)
