{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# language OverloadedStrings #-}
module GDELT.Download where

import Control.Monad (void)
import Data.Functor (($>))

-- megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char (string)
-- req
import Network.HTTP.Req (MonadHttp(..), req, http, (/:), GET, NoReqBody, bsResponse, Url, Scheme(..))
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)


{-
Download links to latest data from http://data.gdeltproject.org/gdeltv2/lastupdate.txt

e.g.

115005 a2d6c712e615cb794fcca976f405a330 http://data.gdeltproject.org/gdeltv2/20200327131500.export.CSV.zip
172895 ec50a51100305087dccbf189e6beb36b http://data.gdeltproject.org/gdeltv2/20200327131500.mentions.CSV.zip
8474162 03accc9154c3471928e4a8089a204c3b http://data.gdeltproject.org/gdeltv2/20200327131500.gkg.csv.zip
-}

lastUpdateUrl :: Url 'Http
lastUpdateUrl = http "data.gdeltproject.org" /: "gdeltv2" /: "lastupdate.txt"

data GDDataset a = GDExport a
                 | GDMentions a
                 | GDGKG a
                 deriving (Eq, Show, Functor)

data GDSource a = GDSource { gdsSize :: Int, gdsHash :: Text, gdsType :: GDDataset a } deriving (Eq, Show, Functor)



-- http://data.gdeltproject.org/gdeltv2/20200327131500.gkg.csv.zip

-- gkgP pref = do
--   p <- string pref
--   void
