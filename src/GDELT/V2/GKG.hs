{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-|
http://data.gdeltproject.org/documentation/GDELT-Global_Knowledge_Graph_Codebook-V2.1.pdf

This  codebook  introduces  the  GDELT  Global  Knowledge  Graph  (GKG) Version  2.1,  which  expands GDELT’s ability to quantify global human society beyond cataloging physical occurrences towards actually representing all of the latent dimensions, geography, and network structure of the global news.  It  applies  an array  of  highly  sophisticated  natural  language  processing  algorithms to  each  document  to compute a range of codified metadata encoding key latent and contextual dimensions of the document.  To sum up the GKG in a single sentence, it connects every person, organization, location, count, theme, news source, and event across the planet into a single massive network that captures what’s happening around the world, what its context is and who’s involved, and how the world is feeling about it, every single day.

-}
module GDELT.V2.GKG where

import Control.Applicative (Alternative(..))
import Data.Functor (($>), void)
import GHC.Generics (Generic(..))


-- megaparsec
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, parseTest, count, option, optional, sepBy, eof)
import Text.Megaparsec (MonadParsec(..))
import Text.Megaparsec.Char (string, char, digitChar, letterChar, punctuationChar, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal, float, signed)
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
-- time
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar (Day, fromGregorian)


import GDELT.V2.Parsec.Common (Parser, ParseError, localTime, digit, signedDouble, hash, semicolon,  ADM1(..), adm1, Latitude(..), latitude, Longitude(..), longitude)
import GDELT.V2.GKG.Themes (ThemesV1(..), themesV1)






{- | GKGRECORDID

(string)  Each  GKG  record  is  assigned  a  globally  unique  identifier.Unlike  the EVENT  system,  which  uses  semi-sequential  numbering  to  assign  numeric  IDs  to  each  event record, the GKG system uses a date-oriented serial number.  Each GKG record ID takes the form “YYYYMMDDHHMMSS-X” or “YYYYMMDDHHMMSS-TX” in which the first portion of the ID is the full  date+time  of  the  15  minute  update  batch  that  this  record  was  created  in,  followed  by  a dash,  followed  by  sequential  numbering  for  all  GKG  records  created  as  part  of  that  update batch. Records originating from a document that was translated by GDELT Translingual will have a capital “T” appearing immediately after the dash to allow filtering of English/non-English material simply by its record identifier.  Thus, the fifth GKG record created as part of the update batch   generated at   3:30AM   on   February   3,   2015   would   have   a   GKGRECORDID   of “20150203033000-5”and if it was based on a French-language document that was translated, it would have the ID "20150203033000-T5".   This  ID  can  be  used  to  uniquely  identify  this particular  record across  the  entire  GKG  database.
-}

data RecordId = RecordId {
    riDate :: LocalTime
  , riTranslingual :: Bool
  , riSeqNo :: Int
  } deriving (Eq, Show, Generic)

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

As an example    of    how    to    interpret    this    file,    an    entry    with    CountType=KILL,    Number=47, ObjectType=”jihadists” indicates that the article stated that 47 jihadists were killed.  This field is identical in format and population as the correspondingfield in the GKG 1.0 format.

- Count  Type.(text)   This  is  the  value  of  the  NAME  field  from  the  Category  List spreadsheet indicating which category this count is of. At the time of this writing, this is most  often  AFFECT,  ARREST,  KIDNAP,  KILL,  PROTEST,  SEIZE,  or  WOUND,  though  other categories  may  appear  here  as  well  in  certain  circumstances  when  they  appear  in context with one of these categories, or as other Count categories are added over time.  A value of “PROTEST” in this field would  indicatethat  this  is  a  count  of  the  number  of protesters at a protest.

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

v21countsEx :: Text
v21countsEx = "WOUND#3#patients#2#Alaska, United States#US#USAK#61.385#-152.268#AK;"

data CountTy = CTAffect | CTArrest | CTKidnap | CTKill | CTProtest | CTSeize | CTWound | CTOther Text deriving (Eq, Show, Generic)

countTy :: Parser CountTy
countTy = (string "AFFECT" $> CTAffect) <|>
  (string "ARREST" $> CTArrest) <|>
  (string "KIDNAP" $> CTKidnap) <|>
  (string "KILL" $> CTKill) <|>
  (string "PROTEST" $> CTProtest) <|>
  (string "SEIZE" $> CTSeize) <|>
  (string "WOUND" $> CTWound)
  -- (string "AFFECT" $> CTAffect)

countsV1 :: Parser CountsV1
countsV1 = do
  cty <- countTy
  hash
  n <- decimal
  hash
  oty <- optional (pack <$> many letterChar)
  hash
  loc <- locationV1
  pure $ CountsV1 cty n oty loc

data CountsV1 = CountsV1 {
    cCountTy :: CountTy
  , cCount :: Int
  , cObjectType :: Maybe Text
  , cLocationV1 :: LocationV1
  } deriving (Eq, Show, Generic)



{-|
V2.1COUNTS.(semicolon-delimited blocks, with pound symbol (“#”) delimited fields)  This field is identical to the V1COUNTS field except that it adds a final additional field to the end of each entry that records its approximate character offset in the document, allowing it to be associated with other entries from other “V2ENHANCED” fields (or Events) that appear in closest proximity to  it.   Note:unlike  the  other  location-related  fields,  the  Counts  field  does  NOT  add  ADM2 support at this time.  This is to maintain compatibility with assumptions that many applications make  about  the  contents  of  the  Count  field.    Those  applications  needing  ADM2  support  for Counts should cross-reference the FeatureID field of a given Count against the V2Locations field to determine its ADM2 value.
-}

data CountsV21 = CountsV21 {
    c21countsV1 :: CountsV1
  , c21charOffset :: Int
                           } deriving (Eq, Show, Generic)

countsV21 :: Parser CountsV21
countsV21 = do
  c1 <- countsV1
  hash
  i <- decimal
  pure $ CountsV21 c1 i




{-|
V1THEMES.(semi-colon-delimited)  This is the list of all Themes found in the document.  For the complete list of possible themes, see the Category List spreadsheet.  At the time of this writing there are over 275 themes currently recognized by the system.    This field is identical in format and population as the corresponding field in the GKG1.0 format.
-}

themesV1List :: Parser [ThemesV1]
themesV1List = sepBy themesV1 semicolon


{-|
V2ENHANCEDTHEMES.(semicolon-delimited   blocks,   with   comma-delimited   fields)      This contains a list of all GKG themes referenced in the document, along with the character offsets of approximately  where  in  the  document  they  were  found.    For  the  complete  list  of  possible themes,  see  the  Category  List  spreadsheet.    At  the  time  of  this  writing  there  are  over  300 themes currently recognized by the system.  Each theme reference is separated by a semicolon, and within each reference, the name of the theme is specified first,  followed by a comma, and then the approximate character offset of the reference of that theme in the document, allowing it  to  be associated with other entries from other “V2ENHANCED” fields that appear in closest proximity to it.  If a theme is mentioned multiple times in a document, each mention will appear separately in this field.
-}

themesV2enhanced :: Parser [(ThemesV1, Int)]
themesV2enhanced = sepBy p semicolon
  where
    p = (,) <$> themesV1 <*> decimal



{-|
V1LOCATIONS.(semicolon-delimited blocks, with pound symbol (“#”) delimited fields)  This is a list  of  all  locations  found  in  the  text,  extracted  through  the  Leetaru  (2012)  algorithm. 2The algorithm  is  run  in  a  more  aggressive  stance  here  than  ordinary  in  order  to extract  every possible locative  referent, so may have  a slightly elevated level of false  positives.  NOTE:some locations have multiple accepted formal or informal names and this field is collapsed on name, rather than feature (since in some applications the understanding of a geographic feature differs based  on  which  name  was  used  to  reference  it).    In  cases  where  it  is  necessary  to  collapse  by feature, the Geo_FeatureID column should be used, rather than the Geo_Fullname column.  This is because the Geo_Fullname column captures the name of the location as expressed in the text and  thus  reflects  differences  in  transliteration,  alternative  spellings,  and  alternative  names  for the  same  location.    For  example,  Mecca  is  often  spelled  Makkah,  while  Jeddah  is  commonly spelled  Jiddah  or  Jaddah.    The  Geo_Fullname  column  will  reflect  each  of  these  different spellings, while the Geo_FeatureID column will resolve them all to the same unique GNS or GNIS feature  identification  number.    For  more  information  on  the  GNS  andGNIS  identifiers,  see Leetaru (2012). 3This field is identical in format and population as the corresponding field in the GKG  1.0 format.NOTE:there  was  an  error in  this  field  from  2/19/2015  through midday 3/1/2015that caused the CountryCode field to listthe wrong country code in somecases.

- Location Type.  (integer) This field specifies the geographic resolution of the match type and  holds  one  of  the  following  values:    1=COUNTRY  (match  was  at  the  country  level), 2=USSTATE  (match was to a US state), 3=USCITY  (match was  to a US city or landmark), 4=WORLDCITY  (match  was  to  a  city  or  landmark  outside  the  US),  5=WORLDSTATE (match was to an Administrative Division 1 outside the US –roughly equivalent to a US state).    This  can  be  used  to  filter  counts  by  geographic  specificity,  for  example, extracting  only  those  counts  with  a  landmark-level  geographic  resolution  for  mapping.  Note that matches with codes 1 (COUNTRY), 2 (USSTATE), and 5 (WORLDSTATE) will still provide a latitude/longitude pair, which will be the centroid of that country or state, but the  FeatureID  field  below  will  contain  its  textual  country  or  ADM1  code  instead  of  a numeric featureid.

- Location  FullName.(text)  This  is  the  full  human-readable  name  of  the  matched location.  In the case of a country it is simply the country name.  For US and World states it is in the format of “State, Country Name”, while for all other matches it is in the format  of “City/Landmark, State, Country”.  This can be used to label locations when placing counts on a map.  Note: this field reflects the precise name used to refer to the location in the text itself, meaning it may contain multiple spellings of the same location –use the FeatureID column to determine whether two location names refer to the same place.

- Location  CountryCode.    (text)  This  is  the  2-character  FIPS10-4  country  code  for  the location.   Note:GDELT  continues  to  use  the  FIPS10-4  codes  under  USG  guidance while GNS  continues  its  formal  transitionto the  successor  Geopolitical  Entities,  Names,  and Codes (GENC) Standard (the US Government profile of ISO 3166).

- Location  ADM1Code.    (text)  This  is  the  2-character  FIPS10-4  country  code  followed  by the  2-character  FIPS10-4  administrative  division  1  (ADM1)  code  for  the  administrative division housing the landmark.  In the case  of the United States,  this is the 2-character shortform of the state’s name (such as “TX” for Texas).Note:see the notice above for CountryCode regarding the FIPS10-4 / GENC transition.  Note: to obtain ADM2 (district-level) assignments for locations, you can either perform a spatial join against a ShapeFile template in any GIS software, or cross-walk the FeatureID to the GNIS/GNS databases –this  will  provide  additional  fields  such as  ADM2  codes  and MGRS  grid  references for GNS.

- Location Latitude.  (floating point number) This is the centroid latitude of the landmark for  mapping.In  the  case  of  a  country  or  administrative  division  this  will  reflect  the centroid of that entire country/division.

- Location  Longitude.    (floating  point  number)  This  is  the  centroid  longitude  of  the landmark for mapping.In the case of a country or administrative division this will reflect the centroid of that entire country/division.

- Location FeatureID.  (text OR signed integer) This is the numeric GNS or GNIS FeatureID for this locationOR a textual country or ADM1 code.  More information onthese values can be found in Leetaru (2012).4Note: This field will be blank or contain a textual ADM1 code  for country or ADM1-level matches –see  above.  Note: For numeric GNS or GNIS FeatureIDs,  this field  can  contain  both  positive  and  negative  numbers,  see  Leetaru (2012) for more information on this.
-}

v1locationEx :: Text
v1locationEx = "2#California, United States#US#USCA#36.17#-119.746#CA"

data LocationTy = LTCountry | LTUSState | LTUSCity | LTWorldCity | LTWorldState deriving (Eq, Show, Enum, Generic)

locationTy :: Parser LocationTy
locationTy = toEnum . subtract 1 <$> decimal

locationFullName :: Parser Text
locationFullName = pack <$> many (letterChar <|> char ',' <|> spaceChar)

locationCountryCode :: Parser Text
locationCountryCode = pack <$> count 2 letterChar

featureId :: Parser (Either Text Int)
featureId = (Left . pack <$> count 2 letterChar) <|>
            (Right <$> decimal)

locationV1 :: Parser LocationV1
locationV1 = do
  lty <- locationTy
  hash
  lfn <- locationFullName
  hash
  lcc <- locationCountryCode
  hash
  a <- adm1
  hash
  lat <- latitude
  hash
  lon <- longitude
  hash
  fid <- featureId
  pure $ LocationV1 lty lfn lcc a lat lon fid


data LocationV1 = LocationV1 {
    locTy :: LocationTy
  , locFullName :: Text
  , locCountryCode :: Text
  , locADM1Code :: ADM1
  , locLat :: Latitude
  , locLon :: Longitude
  , locFeatureId :: Either Text Int
  } deriving (Eq, Show, Generic)




