module BaseSpec (spec) where

import Base
import Test.Hspec

spec :: Spec
spec = do
  describe "Base.complement" $ do
    it "returns the complement of Base A" $ do
      complement A `shouldBe` T

    it "returns the complement of Base T" $ do
      complement T `shouldBe` A

    it "returns the complement of Base G" $ do
      complement G `shouldBe` C

    it "returns the complement of Base C" $ do
      complement C `shouldBe` G
