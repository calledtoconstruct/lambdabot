{-# LANGUAGE ViewPatterns #-}

-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work

-- Joseph Woolley 2021-01-24 converted to use http-conduit and xml-cursor

{- | Search various things, Wikipedia and google for now.

 (c) 2005 Samuel Bronson
 (c) 2006 Don Stewart
-}
module Lambdabot.Plugin.Reference.Search (searchPlugin) where

import Lambdabot.Plugin (
  Command (help, process),
  LB,
  Module (moduleCmds),
  MonadLB (..),
  command,
  getCmdName,
  newModule,
  say,
 )
import Lambdabot.Util (strip)
import Lambdabot.Util.Browser (doHttpRequest)

import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (mk)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.XML.Types (Name (Name))
import Network.HTTP (urlEncode)
import Network.HTTP.Client.Conduit (Request (redirectCount))
import qualified Network.HTTP.Simple as S (parseRequest)
import Network.HTTP.Types.Header (HeaderName)
import Text.Printf (printf)
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, element, hasAttribute, ($.//), ($//), (>=>))

engines :: [(String, (String, String -> String))]
engines =
  [ ("google", (googleUri, \s -> "?hl=en&q=" ++ s ++ "&btnI=I'm+Feeling+Lucky"))
  , ("wikipedia", (wikipediaUri, ("?search=" ++)))
  , ("imdb", ("", ("" ++)))
  ]

googleUri :: String
googleUri = "https://www.google.com/search"

wikipediaUri :: String
wikipediaUri = "https://en.wikipedia.org/wiki/Special:Search"

makeUrl :: String -> String -> String -> String
makeUrl uri queryFormat value = uri ++ printf queryFormat value

searchPlugin :: Module ()
searchPlugin =
  newModule
    { moduleCmds =
        return
          [ (command name)
            { help = say (moduleHelp name)
            , process = \e -> do
                s <- getCmdName
                lb (searchCmd s (strip isSpace e)) >>= mapM_ say
            }
          | name <- map fst engines
          ]
    }

moduleHelp :: String -> String
moduleHelp s = case s of
  "google" -> "google <expr>. Search google and show url of first hit"
  "wikipedia" -> "wikipedia <expr>. Search wikipedia and show url of first hit"
  "imdb" -> "imdb <expr>.  Search imdb for a movie, show, or actor"
  _ -> "Search Plugin does not have command \"" ++ s ++ "\""

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _ [] = return ["Empty search."]
searchCmd engineName (urlEncode -> query)
  | engineName == "google" = do
    request <- S.parseRequest $ makeUrl googleUri "?hl=en&q=%s&btnI=I'm+Feeling+Lucky" query
    doHttpRequest request $ \statusCode body _ ->
      if statusCode == 200
        then
          let href = body $// (element anchorElementName >=> attribute hrefAttributeName)
           in return $ head $ map T.unpack href
        else return "No Result Found."
  | engineName == "wikipedia" = do
    request' <- S.parseRequest $ makeUrl wikipediaUri "?search=%s&ns0=1" query
    let request = request'{redirectCount = 0}
    doHttpRequest request $ \statusCode body responseHeaders ->
      if statusCode == 302
        then return $ getLocationHeader responseHeaders
        else
          if statusCode == 200
            then
              let searchUrl = concatMap T.unpack $ body $// (element linkElementName >=> attributeIs relAttributeName (T.pack "canonical") >=> attribute hrefAttributeName)
                  searchResult = "wikipedia does not have an exact match.  However, I will list the top three results from this search: " ++ searchUrl
                  dataSearchPositionAttributeName = Name (T.pack "data-serp-pos") Nothing Nothing
                  topResults = map (("https://en.wikipedia.org" ++) . T.unpack) $ body $// (element anchorElementName >=> hasAttribute dataSearchPositionAttributeName >=> attribute hrefAttributeName)
               in return $ unlines $ take 4 $ searchResult : topResults
            else return $ show statusCode ++ ": No Result Found."
  | engineName == "imdb" = do
    request <- S.parseRequest $ "https://www.imdb.com/find?s=tt&q=" ++ query ++ "&ref_=nv_sr_sm"
    doHttpRequest request $ \statusCode body _ ->
      if statusCode == 200
        then
          let resultTable = head $ body $// element tableElementName >=> attributeIs classAttributeName (T.pack "findList")
              rows = resultTable $// element rowElementName
              entries = map getEntry rows
              getEntry :: Cursor -> (String, String)
              getEntry cur =
                ( (T.unpack . T.dropWhile (' ' ==) . T.concat) $ cur $.// content
                , (("https://www.imdb.com" ++) . T.unpack . head) $ cur $// element anchorElementName >=> attribute hrefAttributeName
                )
           in return $ unlines $ map (uncurry (++)) $ take 3 entries
        else return $ show statusCode ++ ": No Result Found."
  | otherwise = do
    return ["Unknown search engine requested."]

getLocationHeader :: [(HeaderName, B.ByteString)] -> String
getLocationHeader responseHeaders =
  let locationHeaderName = mk $ B.pack "location"
      locationHeader = head $ filter (\x -> fst x == locationHeaderName) responseHeaders
   in B.unpack $ snd locationHeader

linkElementName :: Name
linkElementName = Name (T.pack "link") Nothing Nothing
relAttributeName :: Name
relAttributeName = Name (T.pack "rel") Nothing Nothing
hrefAttributeName :: Name
hrefAttributeName = Name (T.pack "href") Nothing Nothing
anchorElementName :: Name
anchorElementName = Name (T.pack "a") Nothing Nothing
tableElementName :: Name
tableElementName = Name (T.pack "table") Nothing Nothing
classAttributeName :: Name
classAttributeName = Name (T.pack "class") Nothing Nothing
rowElementName :: Name
rowElementName = Name (T.pack "tr") Nothing Nothing
