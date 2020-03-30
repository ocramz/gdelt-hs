{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-| GKG themes from : 
http://data.gdeltproject.org/documentation/GDELT-Global_Knowledge_Graph_CategoryList.xlsx
-}
module GDELT.V2.GKG.Themes (ThemesV1(..), themesV1) where

import Control.Applicative (Alternative(..))
import Data.Functor (($>))
import GHC.Generics (Generic(..))

import Text.Megaparsec.Char (string)

import GDELT.V2.Parsec.Common (Parser)


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
