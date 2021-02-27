{-# LANGUAGE ViewPatterns #-}

-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work

-- Joseph Woolley 2021-01-24 converted to use http-conduit and xml-cursor
-- Joseph Woolley 2021-01-26 added imdb for movies and discogs for music

{- | Search various things, Wikipedia and google for now.

 (c) 2005 Samuel Bronson
 (c) 2006 Don Stewart
-}
module Lambdabot.Plugin.Reference.Search (searchPlugin) where

import Lambdabot.Config.Reference (enableDiscogs, enableGoogle, enableImdb, enableWikipedia)
import Lambdabot.Plugin (
  Command (help, process),
  LB,
  Module (moduleCmds),
  MonadLB (..),
  MonadConfig (..),
  command,
  getCmdName,
  newModule,
  say,
 )
import Lambdabot.Util (strip)
import Lambdabot.Util.Browser (HttpResponseHandler, RequestOverride, doHttpRequest)

import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (mk)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.XML.Types (Name (Name))
import Network.HTTP (urlEncode)
import Network.HTTP.Conduit (Request (redirectCount))
import qualified Network.HTTP.Simple as S (parseRequest)
import Network.HTTP.Types.Header (HeaderName)
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, element, hasAttribute, ($.//), ($//), (>=>))

engines :: [(String, (LB Bool, String, String -> String, HttpResponseHandler [String], RequestOverride, String))]
engines =
  [ ("google", (isGoogleEnabled, googleSearchUri, ("?hl=en&btnI=I'm+Feeling+Lucky&q=" ++), googleResponseHandler, Nothing, "google <expr>. Search google and show url of first hit"))
  , ("wikipedia", (isWikipediaEnabled, wikipediaSearchUri, ("?ns0=1&search=" ++), wikipediaResponseHandler, Just $ \r -> r{redirectCount = 0}, "wikipedia <expr>. Search wikipedia and show url of first hit"))
  , ("imdb", (isImdbEnabled, imdbSearchUri, ("?&q=" ++), imdbResponseHandler, Nothing, "imdb <expr>.  Search imdb for a movie, show, or actor"))
  , ("imdb-title", (isImdbEnabled, imdbSearchUri, ("?s=tt&q=" ++), imdbResponseHandler, Nothing, "imdb <title>.  Search imdb for a movie or show"))
  , ("imdb-celeb", (isImdbEnabled, imdbSearchUri, ("?s=nm&q=" ++), imdbResponseHandler, Nothing, "imdb <name>.  Search imdb for a celebrity"))
  , ("discogs", (isDiscogsEnabled, discogsSearchUri, ("?type=all&q=" ++), discogsResponseHandler, Nothing, "discogs <expr>.  Search discogs for a song, record, cd, artist, or band"))
  , ("discogs-artist", (isDiscogsEnabled, discogsSearchUri, ("?type=artist&q=" ++), discogsResponseHandler, Nothing, "discogs <name>.  Search discogs for a artist or band"))
  , ("discogs-discs", (isDiscogsEnabled, discogsSearchUri, ("?type=release&q=" ++), discogsResponseHandler, Nothing, "discogs <name>.  Search discogs for a song, record, or cd"))
  ]

isGoogleEnabled :: MonadConfig m => m Bool
isGoogleEnabled = do
  getConfig enableGoogle

isWikipediaEnabled :: MonadConfig m => m Bool
isWikipediaEnabled = do
  getConfig enableWikipedia

isImdbEnabled :: MonadConfig m => m Bool
isImdbEnabled = do
  getConfig enableImdb

isDiscogsEnabled :: MonadConfig m => m Bool
isDiscogsEnabled = do
  getConfig enableDiscogs

discogsRootUri :: String
discogsRootUri = "https://www.discogs.com"

discogsSearchUri :: String
discogsSearchUri = discogsRootUri ++ "/search/"

imdbRootUri :: String
imdbRootUri = "https://www.imdb.com"

imdbSearchUri :: String
imdbSearchUri = imdbRootUri ++ "/find"

googleRootUri :: String
googleRootUri = "https://www.google.com"

googleSearchUri :: String
googleSearchUri = googleRootUri ++ "/search"

wikipediaRootUri :: String
wikipediaRootUri = "https://en.wikipedia.org"

wikipediaSearchUri :: String
wikipediaSearchUri = wikipediaRootUri ++ "/wiki/Special:Search"

searchPlugin :: Module ()
searchPlugin =
  newModule
    { moduleCmds =
        return
          [ (command name)
            { help = say $ moduleHelp name
            , process = \e -> do
                s <- getCmdName
                lb (searchCmd s $ strip isSpace e) >>= mapM_ say
            }
          | name <- map fst engines
          ]
    }

moduleHelp :: String -> String
moduleHelp engineName =
  let (_, _, _, _, _, description) = snd $ head $ filter ((==) engineName . fst) engines
   in description

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _ [] = return ["Empty search."]
searchCmd engineName (urlEncode -> query) = do
  let (isEnabled, uriPrefix, uriSuffixBuilder, handler, maybeOverride, _) = snd $ head $ filter ((==) engineName . fst) engines
  request <- S.parseRequest $ uriPrefix ++ uriSuffixBuilder query
  canCall <- isEnabled
  if canCall
    then doHttpRequest request maybeOverride handler
    else pure []

googleResponseHandler :: HttpResponseHandler [String]
googleResponseHandler statusCode body _
  | statusCode /= 200 = return "No Result Found."
  | otherwise = return $ head $ map T.unpack $ body $// (element anchorElementName >=> attribute hrefAttributeName)

wikipediaResponseHandler :: HttpResponseHandler [String]
wikipediaResponseHandler statusCode body responseHeaders
  | statusCode == 302 = return $ getLocationHeader responseHeaders
  | statusCode == 200 =
    let searchUrl = concatMap T.unpack $ body $// (element linkElementName >=> attributeIs relAttributeName (T.pack "canonical") >=> attribute hrefAttributeName)
        searchResult = "wikipedia does not have an exact match.  However, I will list the top three results from this search: " ++ searchUrl
        dataSearchPositionAttributeName = Name (T.pack "data-serp-pos") Nothing Nothing
        topResults = map ((wikipediaRootUri ++) . T.unpack) $ body $// (element anchorElementName >=> hasAttribute dataSearchPositionAttributeName >=> attribute hrefAttributeName)
     in return $ unlines $ take 4 $ searchResult : topResults
  | otherwise = return $ show statusCode ++ ": No Result Found."

imdbResponseHandler :: HttpResponseHandler [String]
imdbResponseHandler statusCode body _
  | statusCode /= 200 = return $ show statusCode ++ ": No Result Found."
  | noResultsFound = return "No Results Found."
  | otherwise =
    let resultTable = head $ body $// element tableElementName >=> attributeIs classAttributeName (T.pack "findList")
        rows = resultTable $// element rowElementName
        entries = map getEntry rows
     in return $ unlines $ map (uncurry (++)) $ take 3 entries
 where
  noResultsFound = not $ null $ body $// element divElementName >=> attributeIs classAttributeName (T.pack "findNoResults")
  getEntry :: Cursor -> (String, String)
  getEntry cur =
    let label = (T.unpack . T.dropWhile (' ' ==) . T.concat) $ cur $.// content
        url = ((imdbRootUri ++) . T.unpack . head) $ cur $// element anchorElementName >=> attribute hrefAttributeName
     in (label, url)

discogsResponseHandler :: HttpResponseHandler [String]
discogsResponseHandler statusCode body _ = case statusCode of
  200 ->
    let resultTable = head $ body $// element divElementName >=> attributeIs idAttributeName (T.pack "search_results")
        title = map T.unpack $ resultTable $// element anchorElementName >=> attributeIs classAttributeName (T.pack "search_result_title") >=> attribute titleAttributeName
        href = map ((discogsRootUri ++) . T.unpack) $ resultTable $// element anchorElementName >=> attribute hrefAttributeName
     in return $ unlines $ take 3 $ zipWith (\a b -> a ++ " " ++ b) title href
  _ -> return $ show statusCode ++ ": No Result Found."

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
divElementName :: Name
divElementName = Name (T.pack "div") Nothing Nothing
idAttributeName :: Name
idAttributeName = Name (T.pack "id") Nothing Nothing
titleAttributeName :: Name
titleAttributeName = Name (T.pack "title") Nothing Nothing
