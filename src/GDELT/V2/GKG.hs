{-# language DeriveGeneric #-}
{-|
http://data.gdeltproject.org/documentation/GDELT-Global_Knowledge_Graph_Codebook-V2.1.pdf

This  codebook  introduces  the  GDELT  Global  Knowledge  Graph  (GKG)Version  2.1,  which  expands GDELT’s ability to quantify global human society beyond cataloging physical occurrences towards actually representing all of the latent dimensions, geography, and network structure of the global news.  It  applies  an array  of  highly  sophisticated  natural  language  processing  algorithms to  each  document  to compute a range of codified metadata encoding key latent and contextual dimensions of the document.  To sum up the GKG in a single sentence, it connects every person, organization, location, count, theme, news source, and event across the planet into a single massive network that captures what’s happening around the world, what its context is and who’s involved, and how the world is feeling about it, every single day.

-}
module GDELT.V2.GKG where

import Data.Functor (($>))
import GHC.Generics (Generic(..))


-- megaparsec
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, parseTest, count, eof)
import Text.Megaparsec (MonadParsec(..))
import Text.Megaparsec.Char (string, char, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)


import GDELT.V2.Parsec.Common (Parser, ParseError, parseLocalTime)


{- | GKGRECORDID

(string)  Each  GKG  record  is  assigned  a  globally  unique  identifier.Unlike  the EVENT  system,  which  uses  semi-sequential  numbering  to  assign  numeric  IDs  to  each  event record, the GKG system uses a date-oriented serial number.  Each GKG record ID takes the form “YYYYMMDDHHMMSS-X” or “YYYYMMDDHHMMSS-TX” in which the first portion of the ID is the full  date+time  of  the  15  minute  update  batch  that  this  record  was  created  in,  followed  by  a dash,  followed  by  sequential  numbering  for  all  GKG  records  created  as  part  of  that  update batch. Records originating from a document that was translated by GDELT Translingual will have a capital “T” appearing immediately after the dash to allow filtering of English/non-English material simply by its record identifier.  Thus, the fifth GKG record created as part of the update batch   generated at   3:30AM   on   February   3,   2015   would   have   a   GKGRECORDID   of “20150203033000-5”and if it was based on a French-language document that was translated, it would have the ID “20150203033000-T5”.   This  ID  can  be  used  to  uniquely  identify  this particular  record across  the  entire  GKG  database.
-}

data RecordId = RecordId {
    riDate :: LocalTime
  , riTranslingual :: Bool
  , riSeqNo :: Int } deriving (Eq, Show, Generic)

-- parseRecordId r = do
--   dd <- parseLocal

translingual :: Parser Bool
translingual = char 'T' $> True
