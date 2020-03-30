{-# language RankNTypes, TypeFamilies #-}
{-# language DeriveGeneric #-}
module GDELT.V2.Parsec.Common (
  Parser, ParseError
  -- * time
  , localTime, yyyymmdd, tod
  -- * numbers
  , digit
  -- * places
  , ADM1(..), adm1, Latitude(..), latitude, Longitude(..), longitude
  -- * utility
  , signedDouble, hash, semicolon
  ) where

import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Void (Void)
import GHC.Generics (Generic(..))

-- megaparsec
import Text.Megaparsec (MonadParsec(..), Token(..), Parsec, ParseErrorBundle, parseTest, try, count, skipCount)
import Text.Megaparsec.Char (string, char, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer (decimal, float, signed)
-- text
import Data.Text (Text, pack)
-- import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)



type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void


-- * Utility

hash, semicolon :: Parser ()
hash = void $ char '#'
semicolon = void $ char ';'

signedDouble :: RealFloat a => Parser a
signedDouble = signed (pure ()) float


-- *

data ADM1 = ADM1 {
    adm1_FIPS10_4 :: Text
  , adm1Code :: Text
  } deriving (Eq, Show, Generic)

adm1 :: Parser ADM1
adm1 = ADM1 <$> pp <*> pp where
  pp = pack <$> count 2 letterChar


newtype Latitude = Latitude { getLatitude :: Double } deriving (Eq, Show, Generic)
latitude :: Parser Latitude
latitude = Latitude <$> signedDouble

newtype Longitude = Longitude { getLongitude :: Double } deriving (Eq, Show, Generic)
longitude :: Parser Longitude
longitude = Longitude <$> signedDouble

-- | Parse a timestamp in @YYYYMMDDHHMMSS@ format into a 'LocalTime'
localTime :: Parser LocalTime
localTime = LocalTime <$> yyyymmdd <*> tod

yyyymmdd :: Parser Day
yyyymmdd = fromGregorian <$> yd <*> decimalBounded 2 <*> decimalBounded 2
  where
    yd = fromIntegral <$> decimalBounded 4

tod :: Parser TimeOfDay
tod = TimeOfDay <$> decimalBounded 2 <*> decimalBounded 2 <*> sp
  where
    sp = fromIntegral <$> decimalBounded 2


digit :: Num a => Parser a
digit = fromIntegral . digitToInt <$> digitChar

decimalBounded :: Int -> Parser Int
decimalBounded n = do
  ds <- count n digit
  let accf a c = a * 10 + c
  pure $ foldl' accf 0 ds
