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


import GDELT.V2.Parsec.Common (Parser, ParseError, localTime, digit, signedDouble, hash, ADM1(..), adm1, Latitude(..), latitude, Longitude(..), longitude)







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





{-|
V1THEMES.(semi-colon-delimited)  This is the list of all Themes found in the document.  For the complete list of possible themes, see the Category List spreadsheet.  At the time of this writing there are over 275 themes currently recognized by the system.    This field is identical in format and population as the corresponding field in the GKG1.0 format.
-}

data ThemesV1 = ACT_FORCEPOSTURE
              | ACT_HARMTHREATEN
              | ACT_MAKESTATEMENT
              | ACT_YIELD
              | AFFECT
              | AGRICULTURE
              | ALLIANCE
              | APPOINTMENT
              | ARMEDCONFLICT
              | ARREST
              | ASSASSINATION
              | AUSTERITY
              | AVIATION_INCIDENT
              | BAN
              | BLACK_MARKET
              | BLOCKADE
              | BORDER
              | BULLYING
              | CEASEFIRE
              | CHARASMATIC_LEADERSHIP
              | CHECKPOINT
              | CLAIM_CREDIT
              | CLOSURE
              | CONFISCATION
              | CONSTITUTIONAL
              | CORRUPTION
              | CRIME_CARTELS
              | CRIME_COMMON_ROBBERY
              | CRIME_ILLEGAL_DRUGS
              | CURFEW
              | CYBER_ATTACK
              | DEATH_PENALTY
              | DEFECTION
              | DELAY
              | DEMOCRACY
              | DISABILITY
              | DISCRIMINATION
              | DISPLACED
              | DRONE
              | DRUG_TRADE
              | ECON_BANKRUPTCY
              | ECON_BOYCOTT
              | ECON_COST_OF_LIVING
              | ECON_CURRENCY_EXCHANGE_RATE
              | ECON_CURRENCY_RESERVES
              | ECON_CUTOUTLOOK
              | ECON_DEBT
              | ECON_DEREGULATION
              | ECON_EARNINGSREPORT
              | ECON_ENTREPRENEURSHIP
              | ECON_FOREIGNINVEST
              | ECON_FREETRADE
              | ECON_HOUSING_PRICES
              | ECON_INFORMAL_ECONOMY
              | ECON_INTEREST_RATES
              | ECON_IPO
              | ECON_MONOPOLY
              | ECON_MOU
              | ECON_NATIONALIZE
              | ECON_PRICECONTROL
              | ECON_REMITTANCE
              | ECON_STOCKMARKET
              | ECON_SUBSIDIES
              | ECON_TAXATION
              | ECON_TRADE_DISPUTE
              | ECON_UNIONS
              | EDUCATION
              | ELECTION
              | ELECTION_FRAUD
              | ENV_BIOFUEL
              | ENV_CARBONCAPTURE
              | ENV_CLIMATECHANGE
              | ENV_COAL
              | ENV_DEFORESTATION
              | ENV_FISHERY
              | ENV_FORESTRY
              | ENV_GEOTHERMAL
              | ENV_GREEN
              | ENV_HYDRO
              | ENV_METALS
              | ENV_MINING
              | ENV_NATURALGAS
              | ENV_NUCLEARPOWER
              | ENV_OIL
              | ENV_OVERFISH
              | ENV_POACHING
              | ENV_SOLAR
              | ENV_SPECIESENDANGERED
              | ENV_SPECIESEXTINCT
              | ENV_WATERWAYS
              | ENV_WINDPOWER
              | ETH_INDIGINOUS
              | EVACUATION
              | EXHUMATION
              | EXILE
              | EXTREMISM
              | FIREARM_OWNERSHIP
              | FOOD_SECURITY
              | FOOD_STAPLE
              | FREESPEECH
              | FUELPRICES
              | GEN_HOLIDAY
              | GENDER_VIOLENCE
              | GENERAL_GOVERNMENT
              | GENERAL_HEALTH
              | GENTRIFICATION
              | GOV_DISSOLVEGOV
              | GOV_DIVISIONOFPOWER
              | GOV_INTERGOVERNMENTAL
              | GOV_REFORM
              | GOV_REPATRIATION
              | GRIEVANCES
              | HARASSMENT
              | HATE_SPEECH
              | HEALTH_PANDEMIC
              | HEALTH_SEXTRANSDISEASE
              | HEALTH_VACCINATION
              | HUMAN_TRAFFICKING
              | IDEOLOGY
              | IMMIGRATION
              | IMPEACHMENT
              | INFO_HOAX
              | INFO_RUMOR
              | INFRASTRUCTURE_BAD_ROADS
              | INSURGENCY
              | INTERNET_BLACKOUT
              | INTERNET_CENSORSHIP
              | JIHAD
              | KIDNAP
              | KILL
              | LANDMINE
              | LEADER
              | LEGALIZE
              | LEGISLATION
              | LGBT
              | LITERACY
              | LOCUSTS
              | MANMADE_DISASTER
              | MANMADE_DISASTER_IMPLIED
              | MARITIME
              | MARITIME_INCIDENT
              | MARITIME_INCIDENT_IMPLIED
              | MARITIME_INCIDENT_SELF_IDENTIFIED
              | MARITIME_PIRACY
              | MEDIA_CENSORSHIP
              | MEDIA_MSM
              | MEDIA_SOCIAL
              | MEDICAL
              | MEDICAL_SECURITY
              | MIL_SELF_IDENTIFIED_ARMS_DEAL
              | MIL_WEAPONS_PROLIFERATION
              | MILITARY
              | MILITARY_COOPERATION
              | MOVEMENT_ENVIRONMENTAL
              | MOVEMENT_GENERAL
              | MOVEMENT_OTHER
              | MOVEMENT_SOCIAL
              | MOVEMENT_WOMENS
              | NATURAL_DISASTER
              | NEGOTIATIONS
              | NEW_CONSTRUCTION
              | ORGANIZED_CRIME
              | PEACEKEEPING
              | PERSECUTION
              | PHONE_OUTAGE
              | PIPELINE_INCIDENT
              | PIRACY
              | POL_HOSTVISIT
              | POLITICAL_PRISONER
              | POLITICAL_TURMOIL
              | POPULATION_DENSITY
              | POVERTY
              | POWER_OUTAGE
              | PRIVATIZATION
              | PROPAGANDA
              | PROPERTY_RIGHTS
              | PROTEST
              | PUBLIC_TRANSPORT
              | RAIL_INCIDENT
              | RAPE
              | RATIFY
              | REBELLION
              | REBELS
              | RECRUITMENT
              | REFUGEES
              | REL_ANTISEMITISM
              | RELATIONS
              | RELEASE_HOSTAGE
              | RELEASE_PRISON
              | RELIGION
              | RESIGNATION
              | RETALIATE
              | RETIREMENT
              | RETIREMENTS
              | ROAD_INCIDENT
              | RURAL
              | SANCTIONS
              | SANITATION
              | SCANDAL
              | SCIENCE
              | SECURITY_SERVICES
              | SEIGE
              | SEIZE
              | SELF_IDENTIFIED_ATROCITY
              | SELF_IDENTIFIED_ENVIRON_DISASTER
              | SELF_IDENTIFIED_HUMAN_RIGHTS
              | SELF_IDENTIFIED_HUMANITARIAN_CRISIS
              | SEPARATISTS
              | SHORTAGE
              | SICKENED
              | SLFID_CAPACITY_BUILDING
              | SLFID_CIVIL_LIBERTIES
              | SLFID_DICTATORSHIP
              | SLFID_ECONOMIC_DEVELOPMENT
              | SLFID_ECONOMIC_POWER
              | SLFID_MILITARY_BUILDUP
              | SLFID_MILITARY_READINESS
              | SLFID_MILITARY_SPENDING
              | SLFID_MINERAL_RESOURCES
              | SLFID_NATURAL_RESOURCES
              | SLFID_PEACE_BUILDING
              | SLFID_POLITICAL_BOUNDARIES
              | SLFID_RULE_OF_LAW
              | SLUMS
              | SMUGGLING
              | SOC_DIPLOMCOOP
              | SOC_ECONCOOP
              | SOC_EXPRESSREGRET
              | SOC_EXPRESSSUPPORT
              | SOC_FORCEDRELOCATION
              | SOC_GENERALCRIME
              | SOC_INTELSHARING
              | SOC_JUDICIALCOOP
              | SOC_MASSMIGRATION
              | SOC_PARDON
              | SOC_SUICIDE
              | SOC_SUSPICIOUSACTIVITIES
              | SOC_SUSPICIOUSPEOPLE
              | SOC_TRAFFICACCIDENT
              | SOVEREIGNTY
              | STATE_OF_EMERGENCY
              | STRIKE
              | SUICIDE_ATTACK
              | SURVEILLANCE
              | TAKE_OFFICE
              | TAX_CARTELS
              | TAX_DISEASE
              | TAX_ETHNICITY
              | TAX_FNCACT
              | TAX_FOODSTAPLES
              | TAX_MILITARY_TITLE
              | TAX_POLITICAL_PARTY
              | TAX_RELIGION
              | TAX_SPECIAL_ISSUES
              | TAX_SPECIALDEATH
              | TAX_TERROR_GROUP
              | TAX_WEAPONS
              | TERROR
              | TORTURE
              | TOURISM
              | TRAFFIC
              | TRANSPARENCY
              | TREASON
              | TRIAL
              | UNEMPLOYMENT
              | UNGOVERNED
              | UNREST_CHECKPOINT
              | UNREST_CLOSINGBORDER
              | UNREST_HUNGERSTRIKE
              | UNREST_MOLOTOVCOCKTAIL
              | UNREST_POLICEBRUTALITY
              | UNREST_STONETHROWING
              | UNREST_STONING
              | UNSAFE_WORK_ENVIRONMENT
              | URBAN
              | URBAN_SPRAWL
              | VANDALIZE
              | VETO
              | VIOLENT_UNREST
              | WATER_SECURITY
              | WHISTLEBLOWER
              | WMD
              | WOUND
              deriving (Eq, Show, Generic)

themesV1 :: Parser ThemesV1
themesV1 = (string "ACT_FORCEPOSTURE" $>  ACT_FORCEPOSTURE ) <|>
  (string "ACT_HARMTHREATEN" $>  ACT_HARMTHREATEN ) <|>
  (string "ACT_MAKESTATEMENT" $>  ACT_MAKESTATEMENT ) <|>
  (string "ACT_YIELD" $>  ACT_YIELD ) <|>
  (string "AFFECT" $>  AFFECT ) <|>
  (string "AGRICULTURE" $>  AGRICULTURE ) <|>
  (string "ALLIANCE" $>  ALLIANCE ) <|>
  (string "APPOINTMENT" $>  APPOINTMENT ) <|>
  (string "ARMEDCONFLICT" $>  ARMEDCONFLICT ) <|>
  (string "ARREST" $>  ARREST ) <|>
  (string "ASSASSINATION" $>  ASSASSINATION ) <|>
  (string "AUSTERITY" $>  AUSTERITY ) <|>
  (string "AVIATION_INCIDENT" $>  AVIATION_INCIDENT ) <|>
  (string "BAN" $>  BAN ) <|>
  (string "BLACK_MARKET" $>  BLACK_MARKET ) <|>
  (string "BLOCKADE" $>  BLOCKADE ) <|>
  (string "BORDER" $>  BORDER ) <|>
  (string "BULLYING" $>  BULLYING ) <|>
  (string "CEASEFIRE" $>  CEASEFIRE ) <|>
  (string "CHARASMATIC_LEADERSHIP" $>  CHARASMATIC_LEADERSHIP ) <|>
  (string "CHECKPOINT" $>  CHECKPOINT ) <|>
  (string "CLAIM_CREDIT" $>  CLAIM_CREDIT ) <|>
  (string "CLOSURE" $>  CLOSURE ) <|>
  (string "CONFISCATION" $>  CONFISCATION ) <|>
  (string "CONSTITUTIONAL" $>  CONSTITUTIONAL ) <|>
  (string "CORRUPTION" $>  CORRUPTION ) <|>
  (string "CRIME_CARTELS" $>  CRIME_CARTELS ) <|>
  (string "CRIME_COMMON_ROBBERY" $>  CRIME_COMMON_ROBBERY ) <|>
  (string "CRIME_ILLEGAL_DRUGS" $>  CRIME_ILLEGAL_DRUGS ) <|>
  (string "CURFEW" $>  CURFEW ) <|>
  (string "CYBER_ATTACK" $>  CYBER_ATTACK ) <|>
  (string "DEATH_PENALTY" $>  DEATH_PENALTY ) <|>
  (string "DEFECTION" $>  DEFECTION ) <|>
  (string "DELAY" $>  DELAY ) <|>
  (string "DEMOCRACY" $>  DEMOCRACY ) <|>
  (string "DISABILITY" $>  DISABILITY ) <|>
  (string "DISCRIMINATION" $>  DISCRIMINATION ) <|>
  (string "DISPLACED" $>  DISPLACED ) <|>
  (string "DRONE" $>  DRONE ) <|>
  (string "DRUG_TRADE" $>  DRUG_TRADE ) <|>
  (string "ECON_BANKRUPTCY" $>  ECON_BANKRUPTCY ) <|>
  (string "ECON_BOYCOTT" $>  ECON_BOYCOTT ) <|>
  (string "ECON_COST_OF_LIVING" $>  ECON_COST_OF_LIVING ) <|>
  (string "ECON_CURRENCY_EXCHANGE_RATE" $>  ECON_CURRENCY_EXCHANGE_RATE ) <|>
  (string "ECON_CURRENCY_RESERVES" $>  ECON_CURRENCY_RESERVES ) <|>
  (string "ECON_CUTOUTLOOK" $>  ECON_CUTOUTLOOK ) <|>
  (string "ECON_DEBT" $>  ECON_DEBT ) <|>
  (string "ECON_DEREGULATION" $>  ECON_DEREGULATION ) <|>
  (string "ECON_EARNINGSREPORT" $>  ECON_EARNINGSREPORT ) <|>
  (string "ECON_ENTREPRENEURSHIP" $>  ECON_ENTREPRENEURSHIP ) <|>
  (string "ECON_FOREIGNINVEST" $>  ECON_FOREIGNINVEST ) <|>
  (string "ECON_FREETRADE" $>  ECON_FREETRADE ) <|>
  (string "ECON_HOUSING_PRICES" $>  ECON_HOUSING_PRICES ) <|>
  (string "ECON_INFORMAL_ECONOMY" $>  ECON_INFORMAL_ECONOMY ) <|>
  (string "ECON_INTEREST_RATES" $>  ECON_INTEREST_RATES ) <|>
  (string "ECON_IPO" $>  ECON_IPO ) <|>
  (string "ECON_MONOPOLY" $>  ECON_MONOPOLY ) <|>
  (string "ECON_MOU" $>  ECON_MOU ) <|>
  (string "ECON_NATIONALIZE" $>  ECON_NATIONALIZE ) <|>
  (string "ECON_PRICECONTROL" $>  ECON_PRICECONTROL ) <|>
  (string "ECON_REMITTANCE" $>  ECON_REMITTANCE ) <|>
  (string "ECON_STOCKMARKET" $>  ECON_STOCKMARKET ) <|>
  (string "ECON_SUBSIDIES" $>  ECON_SUBSIDIES ) <|>
  (string "ECON_TAXATION" $>  ECON_TAXATION ) <|>
  (string "ECON_TRADE_DISPUTE" $>  ECON_TRADE_DISPUTE ) <|>
  (string "ECON_UNIONS" $>  ECON_UNIONS ) <|>
  (string "EDUCATION" $>  EDUCATION ) <|>
  (string "ELECTION" $>  ELECTION ) <|>
  (string "ELECTION_FRAUD" $>  ELECTION_FRAUD ) <|>
  (string "ENV_BIOFUEL" $>  ENV_BIOFUEL ) <|>
  (string "ENV_CARBONCAPTURE" $>  ENV_CARBONCAPTURE ) <|>
  (string "ENV_CLIMATECHANGE" $>  ENV_CLIMATECHANGE ) <|>
  (string "ENV_COAL" $>  ENV_COAL ) <|>
  (string "ENV_DEFORESTATION" $>  ENV_DEFORESTATION ) <|>
  (string "ENV_FISHERY" $>  ENV_FISHERY ) <|>
  (string "ENV_FORESTRY" $>  ENV_FORESTRY ) <|>
  (string "ENV_GEOTHERMAL" $>  ENV_GEOTHERMAL ) <|>
  (string "ENV_GREEN" $>  ENV_GREEN ) <|>
  (string "ENV_HYDRO" $>  ENV_HYDRO ) <|>
  (string "ENV_METALS" $>  ENV_METALS ) <|>
  (string "ENV_MINING" $>  ENV_MINING ) <|>
  (string "ENV_NATURALGAS" $>  ENV_NATURALGAS ) <|>
  (string "ENV_NUCLEARPOWER" $>  ENV_NUCLEARPOWER ) <|>
  (string "ENV_OIL" $>  ENV_OIL ) <|>
  (string "ENV_OVERFISH" $>  ENV_OVERFISH ) <|>
  (string "ENV_POACHING" $>  ENV_POACHING ) <|>
  (string "ENV_SOLAR" $>  ENV_SOLAR ) <|>
  (string "ENV_SPECIESENDANGERED" $>  ENV_SPECIESENDANGERED ) <|>
  (string "ENV_SPECIESEXTINCT" $>  ENV_SPECIESEXTINCT ) <|>
  (string "ENV_WATERWAYS" $>  ENV_WATERWAYS ) <|>
  (string "ENV_WINDPOWER" $>  ENV_WINDPOWER ) <|>
  (string "ETH_INDIGINOUS" $>  ETH_INDIGINOUS ) <|>
  (string "EVACUATION" $>  EVACUATION ) <|>
  (string "EXHUMATION" $>  EXHUMATION ) <|>
  (string "EXILE" $>  EXILE ) <|>
  (string "EXTREMISM" $>  EXTREMISM ) <|>
  (string "FIREARM_OWNERSHIP" $>  FIREARM_OWNERSHIP ) <|>
  (string "FOOD_SECURITY" $>  FOOD_SECURITY ) <|>
  (string "FOOD_STAPLE" $>  FOOD_STAPLE ) <|>
  (string "FREESPEECH" $>  FREESPEECH ) <|>
  (string "FUELPRICES" $>  FUELPRICES ) <|>
  (string "GEN_HOLIDAY" $>  GEN_HOLIDAY ) <|>
  (string "GENDER_VIOLENCE" $>  GENDER_VIOLENCE ) <|>
  (string "GENERAL_GOVERNMENT" $>  GENERAL_GOVERNMENT ) <|>
  (string "GENERAL_HEALTH" $>  GENERAL_HEALTH ) <|>
  (string "GENTRIFICATION" $>  GENTRIFICATION ) <|>
  (string "GOV_DISSOLVEGOV" $>  GOV_DISSOLVEGOV ) <|>
  (string "GOV_DIVISIONOFPOWER" $>  GOV_DIVISIONOFPOWER ) <|>
  (string "GOV_INTERGOVERNMENTAL" $>  GOV_INTERGOVERNMENTAL ) <|>
  (string "GOV_REFORM" $>  GOV_REFORM ) <|>
  (string "GOV_REPATRIATION" $>  GOV_REPATRIATION ) <|>
  (string "GRIEVANCES" $>  GRIEVANCES ) <|>
  (string "HARASSMENT" $>  HARASSMENT ) <|>
  (string "HATE_SPEECH" $>  HATE_SPEECH ) <|>
  (string "HEALTH_PANDEMIC" $>  HEALTH_PANDEMIC ) <|>
  (string "HEALTH_SEXTRANSDISEASE" $>  HEALTH_SEXTRANSDISEASE ) <|>
  (string "HEALTH_VACCINATION" $>  HEALTH_VACCINATION ) <|>
  (string "HUMAN_TRAFFICKING" $>  HUMAN_TRAFFICKING ) <|>
  (string "IDEOLOGY" $>  IDEOLOGY ) <|>
  (string "IMMIGRATION" $>  IMMIGRATION ) <|>
  (string "IMPEACHMENT" $>  IMPEACHMENT ) <|>
  (string "INFO_HOAX" $>  INFO_HOAX ) <|>
  (string "INFO_RUMOR" $>  INFO_RUMOR ) <|>
  (string "INFRASTRUCTURE_BAD_ROADS" $>  INFRASTRUCTURE_BAD_ROADS ) <|>
  (string "INSURGENCY" $>  INSURGENCY ) <|>
  (string "INTERNET_BLACKOUT" $>  INTERNET_BLACKOUT ) <|>
  (string "INTERNET_CENSORSHIP" $>  INTERNET_CENSORSHIP ) <|>
  (string "JIHAD" $>  JIHAD ) <|>
  (string "KIDNAP" $>  KIDNAP ) <|>
  (string "KILL" $>  KILL ) <|>
  (string "LANDMINE" $>  LANDMINE ) <|>
  (string "LEADER" $>  LEADER ) <|>
  (string "LEGALIZE" $>  LEGALIZE ) <|>
  (string "LEGISLATION" $>  LEGISLATION ) <|>
  (string "LGBT" $>  LGBT ) <|>
  (string "LITERACY" $>  LITERACY ) <|>
  (string "LOCUSTS" $>  LOCUSTS ) <|>
  (string "MANMADE_DISASTER" $>  MANMADE_DISASTER ) <|>
  (string "MANMADE_DISASTER_IMPLIED" $>  MANMADE_DISASTER_IMPLIED ) <|>
  (string "MARITIME" $>  MARITIME ) <|>
  (string "MARITIME_INCIDENT" $>  MARITIME_INCIDENT ) <|>
  (string "MARITIME_INCIDENT_IMPLIED" $>  MARITIME_INCIDENT_IMPLIED ) <|>
  (string "MARITIME_INCIDENT_SELF_IDENTIFIED" $>  MARITIME_INCIDENT_SELF_IDENTIFIED ) <|>
  (string "MARITIME_PIRACY" $>  MARITIME_PIRACY ) <|>
  (string "MEDIA_CENSORSHIP" $>  MEDIA_CENSORSHIP ) <|>
  (string "MEDIA_MSM" $>  MEDIA_MSM ) <|>
  (string "MEDIA_SOCIAL" $>  MEDIA_SOCIAL ) <|>
  (string "MEDICAL" $>  MEDICAL ) <|>
  (string "MEDICAL_SECURITY" $>  MEDICAL_SECURITY ) <|>
  (string "MIL_SELF_IDENTIFIED_ARMS_DEAL" $>  MIL_SELF_IDENTIFIED_ARMS_DEAL ) <|>
  (string "MIL_WEAPONS_PROLIFERATION" $>  MIL_WEAPONS_PROLIFERATION ) <|>
  (string "MILITARY" $>  MILITARY ) <|>
  (string "MILITARY_COOPERATION" $>  MILITARY_COOPERATION ) <|>
  (string "MOVEMENT_ENVIRONMENTAL" $>  MOVEMENT_ENVIRONMENTAL ) <|>
  (string "MOVEMENT_GENERAL" $>  MOVEMENT_GENERAL ) <|>
  (string "MOVEMENT_OTHER" $>  MOVEMENT_OTHER ) <|>
  (string "MOVEMENT_SOCIAL" $>  MOVEMENT_SOCIAL ) <|>
  (string "MOVEMENT_WOMENS" $>  MOVEMENT_WOMENS ) <|>
  (string "NATURAL_DISASTER" $>  NATURAL_DISASTER ) <|>
  (string "NEGOTIATIONS" $>  NEGOTIATIONS ) <|>
  (string "NEW_CONSTRUCTION" $>  NEW_CONSTRUCTION ) <|>
  (string "ORGANIZED_CRIME" $>  ORGANIZED_CRIME ) <|>
  (string "PEACEKEEPING" $>  PEACEKEEPING ) <|>
  (string "PERSECUTION" $>  PERSECUTION ) <|>
  (string "PHONE_OUTAGE" $>  PHONE_OUTAGE ) <|>
  (string "PIPELINE_INCIDENT" $>  PIPELINE_INCIDENT ) <|>
  (string "PIRACY" $>  PIRACY ) <|>
  (string "POL_HOSTVISIT" $>  POL_HOSTVISIT ) <|>
  (string "POLITICAL_PRISONER" $>  POLITICAL_PRISONER ) <|>
  (string "POLITICAL_TURMOIL" $>  POLITICAL_TURMOIL ) <|>
  (string "POPULATION_DENSITY" $>  POPULATION_DENSITY ) <|>
  (string "POVERTY" $>  POVERTY ) <|>
  (string "POWER_OUTAGE" $>  POWER_OUTAGE ) <|>
  (string "PRIVATIZATION" $>  PRIVATIZATION ) <|>
  (string "PROPAGANDA" $>  PROPAGANDA ) <|>
  (string "PROPERTY_RIGHTS" $>  PROPERTY_RIGHTS ) <|>
  (string "PROTEST" $>  PROTEST ) <|>
  (string "PUBLIC_TRANSPORT" $>  PUBLIC_TRANSPORT ) <|>
  (string "RAIL_INCIDENT" $>  RAIL_INCIDENT ) <|>
  (string "RAPE" $>  RAPE ) <|>
  (string "RATIFY" $>  RATIFY ) <|>
  (string "REBELLION" $>  REBELLION ) <|>
  (string "REBELS" $>  REBELS ) <|>
  (string "RECRUITMENT" $>  RECRUITMENT ) <|>
  (string "REFUGEES" $>  REFUGEES ) <|>
  (string "REL_ANTISEMITISM" $>  REL_ANTISEMITISM ) <|>
  (string "RELATIONS" $>  RELATIONS ) <|>
  (string "RELEASE_HOSTAGE" $>  RELEASE_HOSTAGE ) <|>
  (string "RELEASE_PRISON" $>  RELEASE_PRISON ) <|>
  (string "RELIGION" $>  RELIGION ) <|>
  (string "RESIGNATION" $>  RESIGNATION ) <|>
  (string "RETALIATE" $>  RETALIATE ) <|>
  (string "RETIREMENT" $>  RETIREMENT ) <|>
  (string "RETIREMENTS" $>  RETIREMENTS ) <|>
  (string "ROAD_INCIDENT" $>  ROAD_INCIDENT ) <|>
  (string "RURAL" $>  RURAL ) <|>
  (string "SANCTIONS" $>  SANCTIONS ) <|>
  (string "SANITATION" $>  SANITATION ) <|>
  (string "SCANDAL" $>  SCANDAL ) <|>
  (string "SCIENCE" $>  SCIENCE ) <|>
  (string "SECURITY_SERVICES" $>  SECURITY_SERVICES ) <|>
  (string "SEIGE" $>  SEIGE ) <|>
  (string "SEIZE" $>  SEIZE ) <|>
  (string "SELF_IDENTIFIED_ATROCITY" $>  SELF_IDENTIFIED_ATROCITY ) <|>
  (string "SELF_IDENTIFIED_ENVIRON_DISASTER" $>  SELF_IDENTIFIED_ENVIRON_DISASTER ) <|>
  (string "SELF_IDENTIFIED_HUMAN_RIGHTS" $>  SELF_IDENTIFIED_HUMAN_RIGHTS ) <|>
  (string "SELF_IDENTIFIED_HUMANITARIAN_CRISIS" $>  SELF_IDENTIFIED_HUMANITARIAN_CRISIS ) <|>
  (string "SEPARATISTS" $>  SEPARATISTS ) <|>
  (string "SHORTAGE" $>  SHORTAGE ) <|>
  (string "SICKENED" $>  SICKENED ) <|>
  (string "SLFID_CAPACITY_BUILDING" $>  SLFID_CAPACITY_BUILDING ) <|>
  (string "SLFID_CIVIL_LIBERTIES" $>  SLFID_CIVIL_LIBERTIES ) <|>
  (string "SLFID_DICTATORSHIP" $>  SLFID_DICTATORSHIP ) <|>
  (string "SLFID_ECONOMIC_DEVELOPMENT" $>  SLFID_ECONOMIC_DEVELOPMENT ) <|>
  (string "SLFID_ECONOMIC_POWER" $>  SLFID_ECONOMIC_POWER ) <|>
  (string "SLFID_MILITARY_BUILDUP" $>  SLFID_MILITARY_BUILDUP ) <|>
  (string "SLFID_MILITARY_READINESS" $>  SLFID_MILITARY_READINESS ) <|>
  (string "SLFID_MILITARY_SPENDING" $>  SLFID_MILITARY_SPENDING ) <|>
  (string "SLFID_MINERAL_RESOURCES" $>  SLFID_MINERAL_RESOURCES ) <|>
  (string "SLFID_NATURAL_RESOURCES" $>  SLFID_NATURAL_RESOURCES ) <|>
  (string "SLFID_PEACE_BUILDING" $>  SLFID_PEACE_BUILDING ) <|>
  (string "SLFID_POLITICAL_BOUNDARIES" $>  SLFID_POLITICAL_BOUNDARIES ) <|>
  (string "SLFID_RULE_OF_LAW" $>  SLFID_RULE_OF_LAW ) <|>
  (string "SLUMS" $>  SLUMS ) <|>
  (string "SMUGGLING" $>  SMUGGLING ) <|>
  (string "SOC_DIPLOMCOOP" $>  SOC_DIPLOMCOOP ) <|>
  (string "SOC_ECONCOOP" $>  SOC_ECONCOOP ) <|>
  (string "SOC_EXPRESSREGRET" $>  SOC_EXPRESSREGRET ) <|>
  (string "SOC_EXPRESSSUPPORT" $>  SOC_EXPRESSSUPPORT ) <|>
  (string "SOC_FORCEDRELOCATION" $>  SOC_FORCEDRELOCATION ) <|>
  (string "SOC_GENERALCRIME" $>  SOC_GENERALCRIME ) <|>
  (string "SOC_INTELSHARING" $>  SOC_INTELSHARING ) <|>
  (string "SOC_JUDICIALCOOP" $>  SOC_JUDICIALCOOP ) <|>
  (string "SOC_MASSMIGRATION" $>  SOC_MASSMIGRATION ) <|>
  (string "SOC_PARDON" $>  SOC_PARDON ) <|>
  (string "SOC_SUICIDE" $>  SOC_SUICIDE ) <|>
  (string "SOC_SUSPICIOUSACTIVITIES" $>  SOC_SUSPICIOUSACTIVITIES ) <|>
  (string "SOC_SUSPICIOUSPEOPLE" $>  SOC_SUSPICIOUSPEOPLE ) <|>
  (string "SOC_TRAFFICACCIDENT" $>  SOC_TRAFFICACCIDENT ) <|>
  (string "SOVEREIGNTY" $>  SOVEREIGNTY ) <|>
  (string "STATE_OF_EMERGENCY" $>  STATE_OF_EMERGENCY ) <|>
  (string "STRIKE" $>  STRIKE ) <|>
  (string "SUICIDE_ATTACK" $>  SUICIDE_ATTACK ) <|>
  (string "SURVEILLANCE" $>  SURVEILLANCE ) <|>
  (string "TAKE_OFFICE" $>  TAKE_OFFICE ) <|>
  (string "TAX_CARTELS" $>  TAX_CARTELS ) <|>
  (string "TAX_DISEASE" $>  TAX_DISEASE ) <|>
  (string "TAX_ETHNICITY" $>  TAX_ETHNICITY ) <|>
  (string "TAX_FNCACT" $>  TAX_FNCACT ) <|>
  (string "TAX_FOODSTAPLES" $>  TAX_FOODSTAPLES ) <|>
  (string "TAX_MILITARY_TITLE" $>  TAX_MILITARY_TITLE ) <|>
  (string "TAX_POLITICAL_PARTY" $>  TAX_POLITICAL_PARTY ) <|>
  (string "TAX_RELIGION" $>  TAX_RELIGION ) <|>
  (string "TAX_SPECIAL_ISSUES" $>  TAX_SPECIAL_ISSUES ) <|>
  (string "TAX_SPECIALDEATH" $>  TAX_SPECIALDEATH ) <|>
  (string "TAX_TERROR_GROUP" $>  TAX_TERROR_GROUP ) <|>
  (string "TAX_WEAPONS" $>  TAX_WEAPONS ) <|>
  (string "TERROR" $>  TERROR ) <|>
  (string "TORTURE" $>  TORTURE ) <|>
  (string "TOURISM" $>  TOURISM ) <|>
  (string "TRAFFIC" $>  TRAFFIC ) <|>
  (string "TRANSPARENCY" $>  TRANSPARENCY ) <|>
  (string "TREASON" $>  TREASON ) <|>
  (string "TRIAL" $>  TRIAL ) <|>
  (string "UNEMPLOYMENT" $>  UNEMPLOYMENT ) <|>
  (string "UNGOVERNED" $>  UNGOVERNED ) <|>
  (string "UNREST_CHECKPOINT" $>  UNREST_CHECKPOINT ) <|>
  (string "UNREST_CLOSINGBORDER" $>  UNREST_CLOSINGBORDER ) <|>
  (string "UNREST_HUNGERSTRIKE" $>  UNREST_HUNGERSTRIKE ) <|>
  (string "UNREST_MOLOTOVCOCKTAIL" $>  UNREST_MOLOTOVCOCKTAIL ) <|>
  (string "UNREST_POLICEBRUTALITY" $>  UNREST_POLICEBRUTALITY ) <|>
  (string "UNREST_STONETHROWING" $>  UNREST_STONETHROWING ) <|>
  (string "UNREST_STONING" $>  UNREST_STONING ) <|>
  (string "UNSAFE_WORK_ENVIRONMENT" $>  UNSAFE_WORK_ENVIRONMENT ) <|>
  (string "URBAN" $>  URBAN ) <|>
  (string "URBAN_SPRAWL" $>  URBAN_SPRAWL ) <|>
  (string "VANDALIZE" $>  VANDALIZE ) <|>
  (string "VETO" $>  VETO ) <|>
  (string "VIOLENT_UNREST" $>  VIOLENT_UNREST ) <|>
  (string "WATER_SECURITY" $>  WATER_SECURITY ) <|>
  (string "WHISTLEBLOWER" $>  WHISTLEBLOWER ) <|>
  (string "WMD" $>  WMD )


{-|
V2ENHANCEDTHEMES.(semicolon-delimited   blocks,   with   comma-delimited   fields)      This contains a list of all GKG themes referenced in the document, along with the character offsets of approximately  where  in  the  document  they  were  found.    For  the  complete  list  of  possible themes,  see  the  Category  List  spreadsheet.    At  the  time  of  this  writing  there  are  over  300 themes currently recognized by the system.  Each theme reference is separated by a semicolon, and within each reference, the name of the theme is specified first,  followed by a comma, and then the approximate character offset of the reference of that theme in the document, allowing it  to  be associated with other entries from other “V2ENHANCED” fields that appear in closest proximity to it.  If a theme is mentioned multiple times in a document, each mention will appear separately in this field.
-}





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




