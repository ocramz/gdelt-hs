{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
module GDELT.Download where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, parseTest, count, eof)
import Text.Megaparsec.Char (string, char, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
-- req
import Network.HTTP.Req (MonadHttp(..), req, http, (/:), GET, NoReqBody, bsResponse, Url, Scheme(..))
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)


import GDELT.V2.Parsec.Common (Parser, ParseError)

{-
Download links to latest data from http://data.gdeltproject.org/gdeltv2/lastupdate.txt

e.g.

115005 a2d6c712e615cb794fcca976f405a330 http://data.gdeltproject.org/gdeltv2/20200327131500.export.CSV.zip
172895 ec50a51100305087dccbf189e6beb36b http://data.gdeltproject.org/gdeltv2/20200327131500.mentions.CSV.zip
8474162 03accc9154c3471928e4a8089a204c3b http://data.gdeltproject.org/gdeltv2/20200327131500.gkg.csv.zip
-}

lastUpdateUrl :: Url 'Http
lastUpdateUrl = http "data.gdeltproject.org" /: "gdeltv2" /: "lastupdate.txt"

data GDSource = GDSource {
    gdsHash :: Text
  , gdsUrl :: GDDataset
  } deriving (Eq, Show)

data GDDataset = GDDataset {
  gdDate :: LocalTime
  , gdTy :: GDDatasetTy } deriving (Eq, Show)

data GDDatasetTy = GDTExport
                 | GDTMentions
                 | GDTGKG 
                 deriving (Eq)
instance Show GDDatasetTy where
  show = \case
    GDTExport -> "export"
    GDTMentions -> "mentions"
    GDTGKG -> "gkg"

data URLChunks d = URLChunks { ucPrefix :: Text, ucDate :: d, ucDTy :: GDDatasetTy, ucExt :: Text } deriving (Eq)
instance Show d => Show (URLChunks d) where
  show (URLChunks spre sd sty se) = unpack spre <> show sd <> "." <> show sty <> unpack se
 
chunkUrl :: Parser (URLChunks String)
chunkUrl = do
  pref <- string "http://data.gdeltproject.org/gdeltv2/"
  ymd <- count 14 digitChar
  void $ char '.'
  ty <- (string "export" $> GDTExport ) <|> (string "gkg" $> GDTGKG) <|> (string "mentions" $> GDTMentions)
  ext <- string ".csv.zip" <|> string ".CSV.zip"
  pure $ URLChunks pref ymd ty ext

example0 :: Text
example0 = "http://data.gdeltproject.org/gdeltv2/20200327131500.gkg.csv.zip"
example1 :: Text
example1 = "http://data.gdeltproject.org/gdeltv2/20200327131500.mentions.CSV.zip"

-- gdsUrlP :: Parser (GDDatasetTy Text)
-- gdsUrlP = do
--   pref <- string "http://data.gdeltproject.org/gdeltv2/"
--   ymd <- count 14 digitChar
--   void $ char '.'
--   (xsf, s) <- parseTy
--   ext <- string ".csv.zip" <|> string ".CSV.zip"
--   pure $ xsf (pref <> pack ymd <> "." <> s <> ext)




-- parseYMD :: Parser Int
-- parseYMD = count 4 digitChar >>= decimal




-- parseTy :: Parser ((Text -> GDDatasetTy Text), Text)
-- parseTy = (string "export" $> (GDTExport, "export")) <|>
--           (string "mentions" $> (GDTMentions, "mentions")) <|>
--           (string "gkg" $> (GDTGKG, "gkg"))



