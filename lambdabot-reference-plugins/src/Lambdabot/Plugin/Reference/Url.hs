{-# LANGUAGE PatternGuards #-}

-- | Fetch URL page titles of HTML links.
module Lambdabot.Plugin.Reference.Url (urlPlugin) where

import Lambdabot.Plugin (
  Cmd,
  Command (help, privileged, process),
  LB,
  Module (contextual, moduleCmds, moduleDefState, moduleSerialize),
  ModuleT,
  MonadLB,
  command,
  newModule,
  readMS,
  say,
  stdSerial,
  writeMS,
 )
import Lambdabot.Util.Browser (doHttpRequest', doHttpRequest)

import Control.Monad (when, (<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (MonadTrans (lift))
import Data.List (intercalate, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Network.HTTP.Simple as S (parseRequest)
import Text.Regex.TDFA (
  CompOption (caseSensitive),
  MatchResult (mrAfter, mrMatch),
  RegexContext (matchM),
  RegexMaker (makeRegexOpts),
  RegexOptions (defaultCompOpt, defaultExecOpt),
 )
import Text.XML.Cursor (content, laxElement, ($//), (&/))
import qualified Data.ByteString.Lazy.Char8 as LB

urlPlugin :: Module Bool
urlPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "url-title")
              { help = say "url-title <url>. Fetch the page title."
              , process = maybe (say "Url not valid.") (mbSay <=< fetchTitle) . containsUrl
              }
          , (command "tiny-url")
              { help = say "tiny-url <url>. Shorten <url>."
              , process = maybe (say "Url not valid.") (mbSay <=< fetchTiny) . containsUrl
              }
          , (command "url-on")
              { privileged = True
              , help = say "url-on: enable automatic URL summaries"
              , process = const $ do
                  writeMS True
                  say "Url enabled"
              }
          , (command "url-off")
              { privileged = True
              , help = say "url-off: disable automatic URL summaries"
              , process = const $ do
                  writeMS False
                  say "Url disabled"
              }
          ]
    , moduleDefState = return True
    , moduleSerialize = Just stdSerial
    , contextual = \text -> do
        alive <- lift readMS
        when (alive && not (areSubstringsOf ignoredStrings text)) $ case containsUrl text of
          Nothing -> return ()
          Just url
            | length url > 60 -> do
              title <- fetchTitle url
              tiny <- fetchTiny url
              say (intercalate ", " (catMaybes [title, tiny]))
            | otherwise -> mbSay =<< fetchTitle url
    }

mbSay :: Maybe String -> Cmd (ModuleT Bool LB) ()
mbSay = maybe (return ()) say

------------------------------------------------------------------------

-- | The string that I prepend to the quoted page title.
urlTitlePrompt :: String
urlTitlePrompt = "Title: "

-- | Fetch the title of the specified URL.
fetchTitle :: MonadThrow m => MonadLB m => String -> m (Maybe String)
fetchTitle url = do
  request <- S.parseRequest url
  doHttpRequest request $ \statusCode body _ -> case statusCode of
    200 ->
      let title = map T.unpack $ body $// laxElement (T.pack "title") &/ content
          response = last $ "No title found." : title
       in Just response
    _ -> Nothing

-- | base url for fetching tiny urls
tinyurl :: String
tinyurl = "https://tinyurl.com/api-create.php?url="

-- | Fetch the title of the specified URL.
fetchTiny :: MonadThrow m => MonadLB m => String -> m (Maybe String)
fetchTiny url = do
  request <- S.parseRequest $ tinyurl ++ url
  doHttpRequest' request $ \statusCode bodyString _ -> case statusCode of
    200 -> Just $ LB.unpack bodyString
    _ -> Nothing

{- | List of strings that, if present in a contextual message, will
 prevent the looking up of titles.  This list can be used to stop
 responses to lisppaste for example.  Another important use is to
 another lambdabot looking up a url title that contains another
 url in it (infinite loop).  Ideally, this list could be added to
 by an admin via a privileged command (TODO).
-}
ignoredStrings :: [String]
ignoredStrings =
  [ "paste" -- Ignore lisppaste, rafb.net
  , "cpp.sourcforge.net" -- C++ paste bin
  , "HaskellIrcPastePage" -- Ignore paste page
  , "title of that page" -- Ignore others like the old me
  , urlTitlePrompt -- Ignore others like me
  ]

{- | Suffixes that should be stripped off when identifying URLs in
 contextual messages.  These strings may be punctuation in the
 current sentence vs part of a URL.  Included here is the NUL
 character as well.
-}
ignoredUrlSuffixes :: [String]
ignoredUrlSuffixes = [".", ",", ";", ")", "\"", "\1", "\n"]

-- | Searches a string for an embedded URL and returns it.
containsUrl :: String -> Maybe String
containsUrl text = do
  mr <- matchM begreg text
  let kind = mrMatch mr
      rest = mrAfter mr
      url = takeWhile (`notElem` " \n\t\v") rest
  return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
 where
  begreg = makeRegexOpts opts defaultExecOpt "https?://"
  opts = defaultCompOpt{caseSensitive = False}

{- | Utility function to remove potential suffixes from a string.
 Note, once a suffix is found, it is stripped and returned, no other
 suffixes are searched for at that point.
-}
stripSuffixes :: [String] -> String -> String
stripSuffixes [] str = str
stripSuffixes (s : ss) str
  | s `isSuffixOf` str = take (length str - length s) str
  | otherwise = stripSuffixes ss str

{- | Utility function to check of any of the Strings in the specified
 list are substrings of the String.
-}
areSubstringsOf :: [String] -> String -> Bool
areSubstringsOf = flip (any . flip isSubstringOf)
 where
  isSubstringOf s str = any (isPrefixOf s) (tails str)
