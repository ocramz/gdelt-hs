{-# language DeriveGeneric #-}
{-|
http://data.gdeltproject.org/documentation/GDELT-Global_Knowledge_Graph_Codebook-V2.1.pdf

This  codebook  introduces  the  GDELT  Global  Knowledge  Graph  (GKG) Version  2.1,  which  expands GDELT’s ability to quantify global human society beyond cataloging physical occurrences towards actually representing all of the latent dimensions, geography, and network structure of the global news.  It  applies  an array  of  highly  sophisticated  natural  language  processing  algorithms to  each  document  to compute a range of codified metadata encoding key latent and contextual dimensions of the document.  To sum up the GKG in a single sentence, it connects every person, organization, location, count, theme, news source, and event across the planet into a single massive network that captures what’s happening around the world, what its context is and who’s involved, and how the world is feeling about it, every single day.

-}
module GDELT.V2.GKG where

import Data.Functor (($>), void)
import GHC.Generics (Generic(..))


-- megaparsec
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, parseTest, count, option, optional, eof)
import Text.Megaparsec (MonadParsec(..))
import Text.Megaparsec.Char (string, char, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)


import GDELT.V2.Parsec.Common (Parser, ParseError, localTime, digit)


{- | GKGRECORDID

(string)  Each  GKG  record  is  assigned  a  globally  unique  identifier.Unlike  the EVENT  system,  which  uses  semi-sequential  numbering  to  assign  numeric  IDs  to  each  event record, the GKG system uses a date-oriented serial number.  Each GKG record ID takes the form “YYYYMMDDHHMMSS-X” or “YYYYMMDDHHMMSS-TX” in which the first portion of the ID is the full  date+time  of  the  15  minute  update  batch  that  this  record  was  created  in,  followed  by  a dash,  followed  by  sequential  numbering  for  all  GKG  records  created  as  part  of  that  update batch. Records originating from a document that was translated by GDELT Translingual will have a capital “T” appearing immediately after the dash to allow filtering of English/non-English material simply by its record identifier.  Thus, the fifth GKG record created as part of the update batch   generated at   3:30AM   on   February   3,   2015   would   have   a   GKGRECORDID   of “20150203033000-5”and if it was based on a French-language document that was translated, it would have the ID "20150203033000-T5".   This  ID  can  be  used  to  uniquely  identify  this particular  record across  the  entire  GKG  database.
-}

data RecordId = RecordId {
    riDate :: LocalTime
  , riTranslingual :: Bool
  , riSeqNo :: Int } deriving (Eq, Show, Generic)

recordId :: Parser RecordId
recordId = do
  t <- localTime
  void $ char '-'
  tl <- translingual
  i <- decimal
  pure $ RecordId t tl i

translingual :: Parser Bool
translingual = option False (char 'T' $> True)



{-|
V2.1DATE.  (integer)  This is the date in YYYYMMDDHHMMSSformat on which the news media used to constructthis GKG file  was  published.  NOTE that unlike the  main GDELT  event stream files,  this date representsthe  date of publication of the documentfrom which the  information was extracted –if the article discusses events in the past, the date is NOT time-shifted as it is for the GDELT event stream.  This date will be the same for all rows in a file and is redundant from a data  processing  standpoint,  but  is  provided  to  make  it  easier  to  load  GKG  files  directly  into  an SQL database for analysis.  NOTE: for somespecial collections this value may be 0 indicating that the field is either not applicable or not known for those materials.  For example, OCR’d historical document  collections  may  not  have  robust  metadata  on  publication  date.NOTE:  the  GKG  2.0 format  still  encoded  this  date  in  YYYYMMDD  format,  while  under  GKG  2.1  it  is  now  in YYYYMMDDHHMMSS format.
-}

v21date :: Parser (Maybe LocalTime)
v21date = optional localTime


{-|
V2SOURCECOLLECTIONIDENTIFIER.(integer)   This  is  a  numeric  identifier  that  refers  to  the source collection  the document came  from  and  is  used  to  interpret the  DocumentIdentifier  in the next column.  In essence, it specifies how to interpret the DocumentIdentifier to locate the actual document.  At present, it can hold one of the following values:

1 = WEB (The document originates from the open web and the DocumentIdentifier is a fully-qualified URL that can be used to access the document on the web).
 
2  =  CITATIONONLY  (The  document  originates  from  a  broadcast,  print,  or  other  offline source  in  which  only  a  textual  citation  is  available  for  the  document.    In  this  case  the DocumentIdentifier contains the textual citation for the document).

3 = CORE (The document originates from the CORE archive and the DocumentIdentifier contains  its  DOI,  suitable  for  accessing  the  original  document  through  the   CORE website).

4  =  DTIC  (The  document  originates  from  the  DTIC  archive  and  the  DocumentIdentifier contains its DOI, suitable for accessing the original document through the DTIC website).

5= JSTOR (The document originates from the JSTOR archive and the DocumentIdentifier contains  its  DOI,  suitable  for  accessing  the  original  document  through  your  JSTOR subscriptionif your institution subscribes to it).

6 = NONTEXTUALSOURCE (The document originates from a textual proxy (such as closed captioning) of a non-textual information source (such as a video) available via a URL and the DocumentIdentifier provides the URL of the non-textual original source.  At present, this  Collection  Identifier  is  used  for  processing  of  the  closed  captioning  streams  of  the Internet Archive Television News Archive in which each broadcast is available via a URL, but  the  URL offers  access  only  tothe video of  the  broadcast  and  does  not  provide  any access  to  the textual  closed  captioning  used  to  generate  the  metadata.This  code  is used  in  order  to  draw  a  distinction  between  URL-based  textual  material  (Collection Identifier 1 (WEB) and URL-based non-textual material like the Television News Archive).
-}

data SourceCollectionIdentifier = SCIWeb | SCICitationOnly | SCICore | SCIDTIC | SCIJSTOR | SCINonTextualSource deriving (Eq, Show, Enum, Generic)

sourceCollectionIdentifier :: Parser SourceCollectionIdentifier
sourceCollectionIdentifier = toEnum . subtract 1 <$> digit




{-|
V2SOURCECOMMONNAME.    (text)    This  is  a  human-friendly  identifier  of  the  source  of  the document.  For material originating from the open web with a URL this field will contain the top-level domain the page was from.  For BBC Monitoring material it will contain “BBC Monitoring” and for JSTOR material it will contain “JSTOR.”  This field is intended for human display of major sources   as   well   as   for   network   analysis   of   information   flows   by   source,   obviating the requirement to perform domain or other parsing of the DocumentIdentifier field
-}






{-|
V2DOCUMENTIDENTIFIER.(text)This is the unique external identifier for the source document.It  can  be  used  to  uniquely  identify  the  document  and  access  it  if  you  have  the  necessary subscriptions  or  authorizationsand/or  the  document  is  public access.    This  field  can  contain  a range  of values, from  URLs ofopen  web  resourcesto  textual  citationsof  print  or  broadcast materialto    DOI    identifiers    for    various    document    repositories.For    example,    if SOURCECOLLECTION is equal to 1, this field will contain a fully-qualified URL suitable for direct access.  If SOURCECOLLECTION is equal to 2, this field will contain a textual citation akin to what would  appear  in  an  academic  journal  article  referencing  that  document  (NOTE  that  the  actual citation  format  will  vary  (usually  between  APA, Chicago, Harvard,  or  MLA)  depending  on  a number of factors and no assumptions should be made on its precise formatat this time due to the  way  in  which  this  data  is  currently  provided  to  GDELT –future  efforts  will  focus  on normalization  of  this  field  to  a  standard  citation  format).   IfSOURCECOLLECTION  is  3,  the  field will  contain  a  numeric  or alpha-numeric DOI that can be typed into JSTOR’s search engine to access the document if your institution has a JSTOR subscription.
-}





{-|
V1COUNTS.(semicolon-delimited blocks, with pound symbol (“#”) delimited fields)  This is the list  of  Counts  found  in  this document.    Each  Count  found  is  separated  with  a  semicolon,  while  the  fields  within  a  Count  are  separated by the pound symbol (“#”).Unlike  the  primary  GDELT event stream, these records are not issued unique identifier numbers, nor are they dated.

As an example    of    how    to    interpret    this    file,    an    entry    with    CountType=KILL,    Number=47, ObjectType=”jihadists” indicates that the article stated that 47 jihadists were killed.  This field is identical in format and population as the correspondingfield in the GKG 1.0 format.oCount  Type.(text)   This  is  the  value  of  the  NAME  field  from  the  Category  List spreadsheet indicating which category this count is of.

At the time of this writing, this is most  often  AFFECT,  ARREST,  KIDNAP,  KILL,  PROTEST,  SEIZE,  or  WOUND,  though  other categories  may  appear  here  as  well  in  certain  circumstances  when  they  appear  in context with one of these categories, or as other Count categories are added over time.  A value of “PROTEST” in this field would  indicatethat  this  is  a  count  of  the  number  of protesters at a protest.

- Count.(integer)  This is the actual count being reported.  If CountType is “PROTEST” and  Number  is  126,  this means  that  the  source  article contained  a  mention  of  126 protesters.

- Object  Type.(text) This  records  any  identifying  information  as  to  what  the  number refers to.  For example, a mention of “20 Christian missionaries were arrested” will result in “Christian missionaries” being captured here.  This field will be blank in cases where no identifying information could be identified.

- LocationType.  See the documentation for V1Locations below.
- Location FullName.See the documentation for V1Locations below.
- Location CountryCode.See the documentation for V1Locations below.
- Location ADM1Code.See the documentation for V1Locations below.
- Location Latitude.  See the documentation for V1Locations below.
- Location Longitude.  See the documentation for V1Locations below.
- Location FeatureID.  See the documentation for V1Locations below.
-}


