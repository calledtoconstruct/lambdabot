-- | Pull quotes down from alphavantage.
module Lambdabot.Plugin.Reference.Ticker (tickerPlugin) where

import Lambdabot.Plugin (writeMS,
  Cmd,
  Command(privileged, help, process),
  LB,
  Module (moduleCmds),
  ModuleT,
  MonadLB,
  command,
  newModule,
  readMS,
  say,
 )
import Lambdabot.Util.Browser (AlternateHttpResponseHandler, doHttpRequest')

import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List.Split (splitOn)
import Network.HTTP (urlEncode)
import qualified Network.HTTP.Simple as S
import Control.Monad (unless)

type Ticker = ModuleT String LB

tickerPlugin :: Module String
tickerPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "ticker")
              { help = say "ticker symbols.  Look up quotes for symbols"
              , process = tickerCmd
              }
          , (command "ticker-api-key")
              { help = say "set ticker api key."
              , process = writeMS
              , privileged = True
              }
          ]
    }

------------------------------------------------------------------------

-- Fetch several ticker quotes and report them.
tickerCmd :: String -> Cmd Ticker ()
tickerCmd [] = say "Empty ticker."
tickerCmd tickers = do
  apiKey <- readMS
  unless (null apiKey) $ do
    quotes <- getPage $ tickerUrl apiKey $ head $ words tickers
    case quotes of
      [] -> say "No Result Found."
      xs -> mapM_ say xs

tickerUrl :: String -> String -> String
tickerUrl apiKey ticker =
  "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol="
    ++ urlEncode ticker
    ++ "&datatype=csv&apikey="
    ++ apiKey

-- | Fetch a page via HTTP and return its body as a list of lines.
getPage :: MonadThrow m => MonadLB m => String -> m [String]
getPage url = do
  request <- S.parseRequest url
  doHttpRequest' request Nothing getPageResponseHandler

getPageResponseHandler :: AlternateHttpResponseHandler [String]
getPageResponseHandler statusCode body _ = case statusCode of
    200 -> case csv $ map LB.unpack $ LB.lines $ LB.filter (/= '\r') body of
      Nothing -> []
      Just entries ->
        [ symbolEntry ++ ": Last opened at " ++ openEntry
            ++ " and is currently priced at "
            ++ priceEntry
            ++ ".  This represents a change of "
            ++ changeEntry
            ++ " ("
            ++ changePercentEntry
            ++ ").  The high and low for the last trading day were "
            ++ highEntry
            ++ " and "
            ++ lowEntry
            ++ " with a trading volume of "
            ++ volumeEntry
            ++ "."
        ]
       where
        symbolEntry = getEntry "symbol" entries
        openEntry = getEntry "open" entries
        highEntry = getEntry "high" entries
        lowEntry = getEntry "low" entries
        priceEntry = getEntry "price" entries
        volumeEntry = getEntry "volume" entries
        changeEntry = getEntry "change" entries
        changePercentEntry = getEntry "changePercent" entries
        getEntry :: String -> [(String, String)] -> String
        getEntry name = head . map snd . filter ((==) name . fst)
    _ -> [show statusCode, ": Not able to retrieve data."]

{- | Return a list of comma-separated values.
 Quotes allowed in CSV if it's the first character of a field.
-}
csv :: [String] -> Maybe [(String, String)]
csv [headers, values] = Just $ zip (splitOn "," headers) (splitOn "," values)
csv _ = Nothing
