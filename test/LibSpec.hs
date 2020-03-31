{-# language OverloadedStrings #-}
module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Text (Text)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



v21countsEx :: Text
v21countsEx = "WOUND#3#patients#2#Alaska, United States#US#USAK#61.385#-152.268#AK;"

v1locationEx :: Text
v1locationEx = "2#California, United States#US#USCA#36.17#-119.746#CA"
