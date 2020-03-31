{-# language OverloadedStrings #-}
module LibSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck

-- megaparsec
import Text.Megaparsec (parse)
-- text
import Data.Text (Text)

import GDELT.V2.GKG
import GDELT.V2.Parsec.Common -- (Parser, ParseError)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "GDELT.V2.GKG" $ do
    it "Parses a LocationV1" $ 
      parse locationV1 "" v1locationEx `shouldBe` Right (LocationV1 LTUSState "California, United States" "US" (ADM1 "US" "CA") (Latitude 36.17) (Longitude (-119.746)) (Left "CA"))

    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x



v21countsEx :: Text
v21countsEx = "WOUND#3#patients#2#Alaska, United States#US#USAK#61.385#-152.268#AK"

v1locationEx :: Text
v1locationEx = "2#California, United States#US#USCA#36.17#-119.746#CA"
