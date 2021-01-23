-- (c) Mark Wotton
-- Serialisation (c) 2007 Don Stewart

-- | Talk to hot chixxors.
module Lambdabot.Plugin.Novelty.Vixen (vixenPlugin) where

import Lambdabot.Plugin (
  Module,
  command,
  contextual,
  getLambdabotName,
  help,
  modifyMS,
  moduleCmds,
  moduleDefState,
  moduleSerialize,
  newModule,
  privileged,
  process,
  readMS,
  readOnly,
  say,
  showNick,
 )
import Lambdabot.Util (io, random)
import Text.Regex.TDFA (Regex, makeRegex, match)

import Control.Arrow (first)
import Control.Monad (when)
import Data.Binary (Binary (get, put), decode, getWord8, putWord8)
import qualified Data.ByteString.Char8 as P (ByteString, unpack)
import qualified Data.ByteString.Lazy as L (fromStrict)

vixenPlugin :: Module (Bool, String -> IO String)
vixenPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "vixen")
              { help = say "vixen <phrase>. Sergeant Curry's lonely hearts club"
              , process = \txt -> say =<< io . ($ txt) . snd =<< readMS
              }
          , (command "vixen-on")
              { privileged = True
              , help = do
                  me <- showNick =<< getLambdabotName
                  say ("vixen-on: turn " ++ me ++ " into a chatterbot")
              , process = const $ do
                  modifyMS $ \(_, r) -> (True, r)
                  say "What's this channel about?"
              }
          , (command "vixen-off")
              { privileged = True
              , help = do
                  me <- showNick =<< getLambdabotName
                  say ("vixen-off: shut " ++ me ++ "up")
              , process = const $ do
                  modifyMS $ \(_, r) -> (False, r)
                  say "Bye!"
              }
          ]
    , -- if vixen-chat is on, we can just respond to anything
      contextual = \txt -> do
        (alive, k) <- readMS
        when alive $ io (k txt) >>= say
    , moduleDefState = return (False, const (return "<undefined>"))
    , -- suck in our (read only) regex state from disk
      -- compile it, and stick it in the plugin state
      moduleSerialize = Just $
        readOnly $ \bs ->
          let st = decode (L.fromStrict bs)
              compiled =
                map (first makeRegex) (st :: [(String, WTree)])
           in (False, vixen (mkResponses compiled))
    }

------------------------------------------------------------------------

vixen :: (String -> WTree) -> String -> IO String
vixen k key = P.unpack `fmap` randomW (k key)

randomW :: WTree -> IO P.ByteString
randomW (Leaf a) = return a
randomW (Node ls) = random ls >>= randomW

mkResponses :: RChoice -> String -> WTree
mkResponses choices them =
  (\((_, wtree) : _) -> wtree) $ filter (\(reg, _) -> match reg them) choices

------------------------------------------------------------------------
-- serialisation for the vixen state
--
-- The tree of regexes and responses is written in binary form to
-- State/vixen, and we suck it in on module init, then lazily regexify it all

data WTree = Leaf !P.ByteString | Node ![WTree]
  deriving (Show)

instance Binary WTree where
  put (Leaf s) = putWord8 0 >> put s
  put (Node ls) = putWord8 1 >> put ls
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap Leaf get
      1 -> fmap Node get
      _ -> error "Vixen plugin error: unknown tag"

type RChoice = [(Regex, WTree)] -- compiled choices
