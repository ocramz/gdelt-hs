{-# language RankNTypes, TypeFamilies #-}
module GDELT.V2.Parsec.Common (
  Parser, ParseError
  -- * time
  , localTime, yyyymmdd, tod
  -- * numbers
  , digit
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Proxy (Proxy(..))
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec (MonadParsec(..), Token(..), Parsec, ParseErrorBundle, parse, parseTest, try, count, skipCount, eof, chunkToTokens)
import Text.Megaparsec.Char (string, char, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
-- text
import Data.Text (Text, pack, unpack)
-- import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)



type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void



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
