module EnzymeSpec (spec) where

import AminoAcid
import Enzyme
import Strand
import Test.Hspec

enzyme :: Enzyme
enzyme = [rpu, inc, cop, mvr, mvl, swi, lpu, int]

strandA :: Strand
strandA = fromList [T,A,G,A,T,C,C,A,G,T,C,C,A,C,T,C,G,A]

strandB :: Strand
strandB = fromList [T,A,G,A,T,C,C,A,G,T,C,C,A,C,A,T,C,G,A]

strandC :: Strand
strandC = fromList [A,T,G]

spec :: Spec
spec = do
  describe "Enzyme.act" $ do
    it "parses DNA Strands, returning two Strands as product" $ do
      act enzyme strandA `shouldBe` strandB

fromList :: [Base] -> Strand
fromList bs = undefined
