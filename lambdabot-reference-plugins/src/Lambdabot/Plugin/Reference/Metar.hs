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

import Data.Char (isNumber, isAlpha, toUpper)
import qualified Data.Text as T
import Data.XML.Types (Name (Name))
import qualified Network.HTTP.Client.Conduit as S
import Text.XML.Cursor ((>=>), attribute, (&/), content, element, ($//))

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
    doHttpRequest request $ \statusCode body _ -> do
      if statusCode == 200
        then do
          let numberOfResults = read $ T.unpack $ head $ body $// (element dataElementName >=> attribute numberOfResultsAttributeName) :: Int
          let output = case numberOfResults of
                1 -> let
                    station = T.unpack $ head $ body $// (element stationElementName &/ content)
                    latitude = T.unpack $ head $ body $// (element latitudeElementName &/ content)
                    longitude = T.unpack $ head $ body $// (element longitudeElementName &/ content)
                    elevation = T.unpack $ head $ body $// (element elevationElementName &/ content)
                    temperature = T.unpack $ head $ body $// (element temperatureElementName &/ content)
                    dewpoint = T.unpack $ head $ body $// (element dewpointElementName &/ content)
                    precipitation = T.unpack $ last $ T.pack "Zero" : (body $// (element precipitationElementName &/ content))
                    windSpeed = T.unpack $ head $ body $// (element windSpeedElementName &/ content)
                    windDirection = T.unpack $ head $ body $// (element windDirectionElementName &/ content)
                    visibility = T.unpack $ head $ body $// (element visibilityElementName &/ content)
                  in unlines [
                    station ++ ": Sits at " ++ elevation ++ " meters above sea level at (latitude, longitude) of (" ++ latitude ++ ", " ++ longitude ++ ").",
                    station ++ ": Current temperature is " ++ temperature ++ "C with a dewpoint of " ++ dewpoint ++ "C.  Over the last two hours, the precipitation was " ++ precipitation ++ " inches.",
                    station ++ ": Visibility is " ++ visibility ++ " miles with a " ++ generalizeDirection windDirection ++ " wind of " ++ windSpeed ++ " miles per hour."
                    ]
                _ -> "No results for that station."
          return output
        else return $ show statusCode ++ ": No Result Found."
  | otherwise = return ["Please enter a valid metar station code.  The list is available here: https://www.aviationweather.gov/docs/metar/stations.txt"]

generalizeDirection :: String -> String
generalizeDirection degreesString
  | all ((True ==) . isNumber) degreesString = direction
  | otherwise = degreesString
    where degrees = read degreesString :: Float
          angle = flip mod 16 $ floor $ degrees / 22.5
          direction = [
            "North",
            "North North East",
            "North East",
            "East North East",
            "East",
            "East South East",
            "South East",
            "South South East",
            "South",
            "South South West",
            "South West",
            "West South West",
            "West",
            "West North West",
            "North West",
            "North North West"
            ] !! angle

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
