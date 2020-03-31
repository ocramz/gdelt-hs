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
  , signedDouble, hash, semicolon, colon
  ) where

import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Void (Void)
import GHC.Generics (Generic(..))

-- megaparsec
import Text.Megaparsec (Parsec, ParseErrorBundle, count)
import Text.Megaparsec.Char (char, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer (float, signed)
-- text
import Data.Text (Text, pack)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)



type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void


-- * Utility

hash, semicolon, colon :: Parser ()
hash = void $ char '#'
semicolon = void $ char ';'
colon = void $ char ':'

-- | Parses a real, signed floating-point value.
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

-- | Parses a single decimal digit (0 - 9)
digit :: Num a => Parser a
digit = fromIntegral . digitToInt <$> digitChar

-- | Parses an integer having exactly @n@ digits in its decimal representation
decimalBounded :: Int -> Parser Int
decimalBounded n = do
  ds <- count n digit
  let accf a c = a * 10 + c
  pure $ foldl' accf 0 ds
