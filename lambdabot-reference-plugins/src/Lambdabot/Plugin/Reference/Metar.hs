{- | Look up METAR weather records.

 Copyright (c) 2014 Bertram Felgenhauer <int-e@gmx.de>
 GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

 2021-01-24 Rewritten by Joseph Woolley to utilize http-conduit and xml-conduit
-}
module Lambdabot.Plugin.Reference.Metar (metarPlugin) where

import Lambdabot.Plugin (
  Command (help, process),
  LB,
  Module (moduleCmds),
  MonadLB (lb),
  command,
  newModule,
  say,
 )
import Lambdabot.Util.Browser (doHttpRequest)

import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isNumber, toUpper)
import qualified Data.Text as T
import Data.XML.Types (Name (Name))
import qualified Network.HTTP.Client.Conduit as S
import Network.HTTP.Types (HeaderName)
import Text.XML.Cursor (Cursor, attribute, content, element, ($//), (&/), (>=>))

metarPlugin :: Module ()
metarPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "metar")
              { help =
                  say
                    "metar <ICAO airport code>\n\
                    \Look up METAR weather data for given airport."
              , process = \rest -> do
                  lb (doMetar rest) >>= mapM_ say
              }
          ]
    }

addsUri :: String
addsUri = "https://www.aviationweather.gov/adds/dataserver_current/httpparam"

addsSrc :: String -> String
addsSrc code =
  addsUri
    ++ "?dataSource=metars&requestType=retrieve&format=xml&hoursBeforeNow=2\
       \&mostRecentForEachStation=true&stationString="
    ++ code

doMetar :: String -> LB [String]
doMetar code
  | length code == 4 && all isAlpha code = do
    request <- S.parseRequest $ addsSrc (map toUpper code)
    doHttpRequest request extractResult `E.catch` \E.SomeException{} -> do
      return ["I was not able to fetch a result for that request."]
  | otherwise = return ["Please enter a valid metar station code.  The list is available here: https://www.aviationweather.gov/docs/metar/stations.txt"]

extractResult :: Int -> Cursor -> [(HeaderName, B.ByteString)] -> [String]
extractResult statusCode body _ = case statusCode of
  200 -> do
    let numberOfResults = getAttributeValueFrom body dataElementName numberOfResultsAttributeName
    return $ case read numberOfResults :: Int of
      1 ->
        let station = getContentFrom body stationElementName
            latitude = getContentFrom body latitudeElementName
            longitude = getContentFrom body longitudeElementName
            elevation = getContentFrom body elevationElementName
            temperature = getContentFrom body temperatureElementName
            dewpoint = getContentFrom body dewpointElementName
            precipitation = getContentFromWithDefault body precipitationElementName $ T.pack "Zero"
            windSpeed = getContentFrom body windSpeedElementName
            windDirection = getContentFrom body windDirectionElementName
            visibility = getContentFrom body visibilityElementName
         in unlines
              [ station ++ ": Sits at " ++ elevation ++ " meters above sea level at (latitude, longitude) of (" ++ latitude ++ ", " ++ longitude ++ ")."
              , station ++ ": Current temperature is " ++ temperature ++ "C with a dewpoint of " ++ dewpoint ++ "C.  Over the last two hours, the precipitation was " ++ precipitation ++ " inches."
              , station ++ ": Visibility is " ++ visibility ++ " miles with a " ++ generalizeDirection windDirection ++ " wind of " ++ windSpeed ++ " miles per hour."
              ]
      0 -> "No results for that station."
      _ -> "One station at a time, please."
  _ -> return $ show statusCode ++ ": No Result Found."

getContentFrom :: Cursor -> Name -> String
getContentFrom c e = T.unpack $ head $ c $// (element e &/ content)

getContentFromWithDefault :: Cursor -> Name -> T.Text -> String
getContentFromWithDefault c e d = T.unpack $ last $ d : (c $// (element e &/ content))

getAttributeValueFrom :: Cursor -> Name -> Name -> String
getAttributeValueFrom c e a = T.unpack $ head $ c $// (element e >=> attribute a)

generalizeDirection :: String -> String
generalizeDirection degreesString
  | all ((True ==) . isNumber) degreesString = directions !! angle
  | otherwise = degreesString
 where
  degrees = read degreesString :: Float
  angle = flip mod 16 $ floor $ degrees / 22.5

directions :: [String]
directions =
  [ "North"
  , "North North East"
  , "North East"
  , "East North East"
  , "East"
  , "East South East"
  , "South East"
  , "South South East"
  , "South"
  , "South South West"
  , "South West"
  , "West South West"
  , "West"
  , "West North West"
  , "North West"
  , "North North West"
  ]

dataElementName :: Name
dataElementName = Name (T.pack "data") Nothing Nothing

numberOfResultsAttributeName :: Name
numberOfResultsAttributeName = Name (T.pack "num_results") Nothing Nothing

stationElementName :: Name
stationElementName = Name (T.pack "station_id") Nothing Nothing

latitudeElementName :: Name
latitudeElementName = Name (T.pack "latitude") Nothing Nothing

longitudeElementName :: Name
longitudeElementName = Name (T.pack "longitude") Nothing Nothing

temperatureElementName :: Name
temperatureElementName = Name (T.pack "temp_c") Nothing Nothing

dewpointElementName :: Name
dewpointElementName = Name (T.pack "dewpoint_c") Nothing Nothing

windDirectionElementName :: Name
windDirectionElementName = Name (T.pack "wind_dir_degrees") Nothing Nothing

windSpeedElementName :: Name
windSpeedElementName = Name (T.pack "wind_speed_kt") Nothing Nothing

visibilityElementName :: Name
visibilityElementName = Name (T.pack "visibility_statute_mi") Nothing Nothing

precipitationElementName :: Name
precipitationElementName = Name (T.pack "precip_in") Nothing Nothing

elevationElementName :: Name
elevationElementName = Name (T.pack "elevation_m") Nothing Nothing
