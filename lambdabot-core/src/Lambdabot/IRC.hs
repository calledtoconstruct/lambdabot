--

{- | The IRC module processes the IRC protocol and provides a nice API for sending
   and receiving IRC messages with an IRC server.
-}
module Lambdabot.IRC (
  IrcMessage (..),
  joinChannel,
  partChannel,
  getTopic,
  setTopic,
  codepage,
  privmsg,
  quit,
  timeReply,
  pass,
  user,
  setNick,
) where

import Lambdabot.Message (Message (..))
import Lambdabot.Nick (Nick (..))

import Data.Char (chr, isSpace)
import Data.List.Split (splitOn)

import Control.Monad (liftM2)
import Data.List (intercalate)

{- | An IRC message is a server, a prefix, a command and a list of parameters.

 Note that the strings here are treated as lists of bytes!
-}
data IrcMessage = IrcMessage
  { ircMsgServer :: !String
  , ircMsgLBName :: !String
  , ircMsgPrefix :: !String
  , ircMsgCommand :: !String
  , ircMsgParams :: ![String]
  }

instance Show IrcMessage where
  show msg
    | ircMsgCommand msg == "PASS" && head (ircMsgParams msg) /= "***" = show $ msg{ircMsgParams = ["***"]}
    | otherwise =
      let command =
            if null $ ircMsgPrefix msg
              then ircMsgCommand msg
              else ircMsgPrefix msg ++ ircMsgCommand msg
       in concat ["IrcMessage to ", ircMsgServer msg, " command ", command, " (", intercalate ", " (ircMsgParams msg), ")"]

instance Message IrcMessage where
  nick = liftM2 Nick ircMsgServer (takeWhile (/= '!') . ircMsgPrefix)
  server = ircMsgServer
  fullName = dropWhile (/= '!') . ircMsgPrefix
  channels msg =
    let cstr = head $ ircMsgParams msg
     in map
          (Nick (server msg) . (\(x : xs) -> if x == ':' then xs else x : xs))
          (splitOn "," cstr)

  -- solves what seems to be an inconsistency in the parser
  lambdabotName msg = Nick (server msg) (ircMsgLBName msg)

-- | 'mkMessage' creates a new message from server, command, and list of parameters.
mkMessage :: String -> String -> [String] -> IrcMessage
mkMessage svr cmd params =
  IrcMessage
    { ircMsgServer = svr
    , ircMsgPrefix = ""
    , ircMsgCommand = cmd
    , ircMsgParams = params
    , ircMsgLBName = "urk!<outputmessage>"
    }

joinChannel :: Nick -> IrcMessage
joinChannel loc = mkMessage (nTag loc) "JOIN" [nName loc]

partChannel :: Nick -> IrcMessage
partChannel loc = mkMessage (nTag loc) "PART" [nName loc]

getTopic :: Nick -> IrcMessage
getTopic chan = mkMessage (nTag chan) "TOPIC" [nName chan]

setTopic :: Nick -> String -> IrcMessage
setTopic chan topic = mkMessage (nTag chan) "TOPIC" [nName chan, ':' : topic]

{- | Send a message to a channel
>>> privmsg Nick { nTag = "tag", nName = "name" } "/me waves"
IrcMessage to tag command PRIVMSG (name, :ACTION waves)

>>> privmsg Nick { nTag = "tag", nName = "name" } "waves"
IrcMessage to tag command PRIVMSG (name, :waves)
-}
privmsg :: Nick -> String -> IrcMessage
privmsg who msg =
  if action
    then
      mk
        [nName who, ':' : chr 0x1 : ("ACTION " ++ clean_msg ++ [chr 0x1])]
    else mk [nName who, ':' : clean_msg]
 where
  mk = mkMessage (nTag who) "PRIVMSG"
  cleaned_msg = case filter (/= '\CR') msg of
    str@('@' : _) -> ' ' : str
    str -> str
  (clean_msg, action) = case cleaned_msg of
    ('/' : 'm' : 'e' : r) -> (dropWhile isSpace r, True)
    str -> (str, False)

{- | 'codepage' creates a server CODEPAGE message. The input string given is the
   codepage name for current session.
-}
codepage :: String -> String -> IrcMessage
codepage svr theCodePage = mkMessage svr "CODEPAGE" [' ' : theCodePage]

{- | 'quit' creates a server QUIT message. The input string given is the
   quit message, given to other parties when leaving the network.

>>> quit "server" "message"
IrcMessage to server command QUIT (:message)
-}
quit :: String -> String -> IrcMessage
quit svr msg = mkMessage svr "QUIT" [':' : msg]

{- | Construct a privmsg from the CTCP TIME notice, to feed up to
 the @localtime-reply plugin, which then passes the output to
 the appropriate client.

>>> timeReply IrcMessage {ircMsgServer = "server", ircMsgLBName = "urk!<outputmessage>", ircMsgPrefix = "", ircMsgCommand = "USER", ircMsgParams = ["nick-name","localhost","srvr","irc-name"]}
IrcMessage to server command PRIVMSG (nick-name, :@localtime-reply :)
-}
timeReply :: IrcMessage -> IrcMessage
timeReply msg =
  msg
    { ircMsgCommand = "PRIVMSG"
    , ircMsgParams =
        [ head (ircMsgParams msg)
        , ":@localtime-reply "
            ++ nName (nick msg)
            ++ ":"
            ++ init (drop 7 $last $ ircMsgParams msg)
        ]
    }

{- | Make a USER message
>>> user "server" "nick-name" "srvr" "irc-name"
IrcMessage to server command USER (nick-name, localhost, srvr, irc-name)
-}
user :: String -> String -> String -> String -> IrcMessage
user svr nick_ server_ ircname =
  mkMessage svr "USER" [nick_, "localhost", server_, ircname]

{- | Make a PASS message
>>> pass "server" "pA$$w0rP"
IrcMessage to server command PASS (***)
-}
pass :: String -> String -> IrcMessage
pass svr pwd = mkMessage svr "PASS" [pwd]

{- | Make a NICK message from a nickname
>>> setNick Nick { nTag = "tag", nName = "name" }
IrcMessage to tag command NICK (name)
-}
setNick :: Nick -> IrcMessage
setNick nick_ = mkMessage (nTag nick_) "NICK" [nName nick_]
