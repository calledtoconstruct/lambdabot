{-# LANGUAGE ViewPatterns #-}

-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work

{- | Search various things, Wikipedia and google for now.

 (c) 2005 Samuel Bronson
 (c) 2006 Don Stewart
-}
module Lambdabot.Plugin.Reference.Search (searchPlugin) where

import Lambdabot.Config.Reference (configProxy)
import Lambdabot.Plugin (
  Command (help, process),
  LB,
  Module (moduleCmds),
  MonadConfig (getConfig),
  MonadLB (..),
  command,
  getCmdName,
  newModule,
  say,
 )
import Lambdabot.Util (io, strip)
import Lambdabot.Util.Browser (browseLB, urlPageTitle)

import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.XML.Types (Name (Name))
import Network.HTTP (
  HStream,
  Header,
  HeaderName (HdrLocation, HdrReferer),
  NormalizeRequestOptions (normDoClose, normForProxy, normUserAgent),
  Request (..),
  RequestMethod (GET, HEAD),
  Response (Response, rspBody, rspCode, rspHeaders),
  defaultNormalizeRequestOptions,
  lookupHeader,
  mkHeader,
  normalizeRequest,
  simpleHTTP,
  urlEncode,
 )
import Network.HTTP.Proxy (Proxy (NoProxy))
import qualified Network.HTTP.Simple as S (Request, getResponseBody, httpLBS, parseRequest)
import Network.URI (
  URI (uriAuthority, uriPath, uriQuery, uriScheme),
  URIAuth (URIAuth, uriPort, uriRegName, uriUserInfo),
  nullURI,
 )
import Text.HTML.DOM (parseLBS)
import Text.HTML.TagSoup (
  Tag (TagClose, TagOpen, TagText),
  parseTags,
  sections,
 )
import Text.HTML.TagSoup.Match (anyAttr, tagOpen)
import Text.XML.Cursor (attribute, element, fromDocument, ($//), (>=>))

engines :: [(String, (URI, String -> String, [Header]))]
engines =
  [ ("google", (googleUri, \s -> "?hl=en&q=" ++ s ++ "&btnI=I'm+Feeling+Lucky", googleHeaders))
  , ("wikipedia", (wikipediaUri, ("?search=" ++), [])) -- this has changed and Wikipedia requires a User-Agent string
  , ("gsite", (googleUri, \s -> "?hl=en&q=site%3A" ++ s ++ "&btnI=I'm+Feeling+Lucky", googleHeaders))
  , ("gwiki", (googleUri, \s -> "?hl=en&q=site%3Awiki.haskell.org+" ++ s ++ "&btnI=I'm+Feeling+Lucky", googleHeaders))
  ]

googleHeaders :: [Header]
googleHeaders = [mkHeader HdrReferer "https://www.google.com/"]

normalizeOptions :: MonadLB m => m (NormalizeRequestOptions a)
normalizeOptions = do
  proxy' <- getConfig configProxy
  let hasProxy = case proxy' of
        NoProxy -> False
        _ -> True
  return
    defaultNormalizeRequestOptions
      { normDoClose = True
      , normForProxy = hasProxy
      , normUserAgent = Nothing
      } -- there is a default user agent, perhaps we want it?

makeUri :: String -> String -> URI
makeUri regName path =
  nullURI
    { uriScheme = "https:"
    , uriAuthority = Just (URIAuth{uriUserInfo = "", uriRegName = regName, uriPort = ""})
    , uriPath = path
    }

googleUri :: URI
googleUri = makeUri "www.google.com" "/search"

wikipediaUri :: URI
wikipediaUri = makeUri "en.wikipedia.org" "/wiki/Special:Search"

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
  "gsite" -> "gsite <site> <expr>. Search <site> for <expr> using google"
  "gwiki" -> "gwiki <expr>. Search (new) haskell.org wiki for <expr> using google."
  _ -> "Search Plugin does not have command \"" ++ s ++ "\""

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _ [] = return ["Empty search."]
searchCmd engineName (urlEncode -> query)
  | engineName == "google" = do
    request <- S.parseRequest ("https://google.com/search?hl=en&q=" ++ query ++ "&btnI=I'm+Feeling+Lucky")
    doHTTP' request
  | engineName == "google" = do
    -- for Google we do both to get conversions, e.g. for '3 lbs in kg'
    request <- request'
    doHTTP request $ \response ->
      case response of
        Response{rspCode = (3, 0, 2), rspHeaders = (lookupHeader HdrLocation -> Just url)} ->
          doGoogle >>= handleUrl url
        _ -> fmap (\extra -> if null extra then ["No Result Found."] else extra) doGoogle
  | otherwise = do
    request <- request'
    doHTTP request $ \response ->
      case response of
        Response{rspCode = (3, 0, 2), rspHeaders = (lookupHeader HdrLocation -> Just url)} ->
          handleUrl url []
        _ -> return ["No Result Found."]
 where
  handleUrl url extra = do
    title <- browseLB (urlPageTitle url)
    return $ extra ++ maybe [url] (\t -> [url, "Title: " ++ t]) title
  Just (uri, makeQuery, headers) = lookup engineName engines
  request' = do
    opts <- normalizeOptions
    return $
      normalizeRequest opts $
        Request
          { rqURI = uri{uriQuery = makeQuery query}
          , rqMethod = HEAD
          , rqHeaders = headers
          , rqBody = ""
          }
  doGoogle = do
    request <- request'
    doHTTP (request{rqMethod = GET, rqURI = uri{uriQuery = "?hl=en&q=" ++ query}}) $ \response ->
      case response of
        Response{rspCode = (2, _, _), rspBody = (extractConversion -> Just result)} ->
          return [result]
        _ -> return []

doHTTP' :: S.Request -> LB [String]
doHTTP' request = do
  result <- io $ S.httpLBS request
  let body = S.getResponseBody result
  let document = parseLBS body
  let cursor = fromDocument document
  let elementName = Name (T.pack "a") Nothing Nothing
  let attributeName = Name (T.pack "href") Nothing Nothing
  let href = cursor $// (element elementName >=> attribute attributeName)
  return [head $ map T.unpack href]

doHTTP :: HStream a => Request a -> (Response a -> LB [String]) -> LB [String]
doHTTP request handler = do
  result <- io $ simpleHTTP request
  case result of
    Left connError -> return ["Connection error: " ++ show connError]
    Right response -> handler response

-- This is clearly fragile.
extractConversion :: String -> Maybe String
extractConversion (parseTags -> tags) =
  listToMaybe
    [ txt
    | section <- sections (tagOpen ("h2" ==) (anyAttr (\(name, value) -> name == "class" && value == "r"))) tags
    , let txt = take 80 $ strip isSpace $ drop 1 $ dropWhile (/= '=') $ extractText section
    , not (null txt)
    ]

extractText :: [Tag String] -> String
extractText (TagText t : ts) = t ++ extractText ts
extractText (TagOpen "sup" _ : TagText t : TagClose "sup" : ts) = "^" ++ t ++ extractText ts
extractText (TagClose "h2" : _) = ""
extractText (_ : ts) = extractText ts
extractText _ = ""
