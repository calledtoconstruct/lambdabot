-- | Support for quotes
module Lambdabot.Plugin.Novelty.Slap (
  slapPlugin,
) where

import Lambdabot.Plugin (
  Cmd,
  LB,
  Module,
  ModuleT,
  aliases,
  command,
  getLambdabotName,
  getSender,
  help,
  moduleCmds,
  newModule,
  process,
  say,
  showNick,
 )
import Lambdabot.Util (random)

type Slap = ModuleT () LB

slapPlugin :: Module ()
slapPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "slap")
              { aliases = ["smack"]
              , help = say "slap <nick>. Slap someone amusingly."
              , process = slap
              }
          ]
    }

------------------------------------------------------------------------

slap :: String -> Cmd Slap ()
slap "me" = do
  target <- showNick =<< getSender
  slapRandom target
slap "yourself" = do
  target <- showNick =<< getLambdabotName
  slapRandom target
slap target = slapRandom target

slapRandom :: String -> Cmd Slap ()
slapRandom tgt = say . ($ tgt) =<< random slapList

slapList :: [String -> String]
slapList =
  [ ("/me slaps " ++)
  , \x -> "/me smacks " ++ x ++ " about with a large trout"
  , ("/me beats up " ++)
  , \x -> "/me pokes " ++ x ++ " in the eye"
  , \x -> "why on earth would I slap " ++ x ++ "?"
  , \x -> "*SMACK*, *SLAM*, take that " ++ x ++ "!"
  , const "/me activates her slap-o-matic..."
  , ("/me orders her trained monkeys to punch " ++)
  , \x -> "/me smashes a lamp on " ++ possesiveForm x ++ " head"
  , \x ->
      "/me hits "
        ++ x
        ++ " with a hammer, so they breaks into a thousand pieces"
  , ("/me throws some pointy lambdas at " ++)
  , \x -> "/me loves " ++ x ++ ", so no slapping"
  , \x -> "/me would never hurt " ++ x ++ "!"
  , \x -> "go slap " ++ x ++ " yourself"
  , const "I won't; I want to go get some cookies instead."
  , \x -> "I'd rather not; " ++ x ++ " looks rather dangerous."
  , const "I don't perform such side effects on command!"
  , const "stop telling me what to do"
  , \x -> "/me clobbers " ++ x ++ " with an untyped language"
  , \x -> "/me pulls " ++ x ++ " through the Evil Mangler"
  , \x -> "/me secretly deletes " ++ possesiveForm x ++ " source code"
  , \x -> "/me places her fist firmly on " ++ possesiveForm x ++ " jaw"
  , \x -> "/me locks up " ++ x ++ " in a Monad"
  , \x ->
      "/me submits "
        ++ possesiveForm x
        ++ " email address to a dozen spam lists"
  , \x ->
      "/me moulds " ++ x ++ " into a delicous cookie, and places it in her oven"
  , const "/me will count to five..."
  , \x -> "/me jabs " ++ x ++ " with a C pointer"
  , ("/me is overcome by a sudden desire to hurt " ++)
  , \x -> "/me karate-chops " ++ x ++ " into two equally sized halves"
  , ("Come on, let's all slap " ++)
  , \x -> "/me pushes " ++ x ++ " from his chair"
  , \x -> "/me hits " ++ x ++ " with an assortment of kitchen utensils"
  , \x -> "/me slaps " ++ x ++ " with a slab of concrete"
  , ("/me puts on her slapping gloves, and slaps " ++)
  , \x ->
      "/me decomposes "
        ++ x
        ++ " into several parts using the Banach-Tarski theorem and reassembles them to get two copies of "
        ++ x
        ++ "!"
  ]

-- | The possesive form of a name, "x's"
possesiveForm :: String -> String
possesiveForm [] = []
possesiveForm x
  | last x == 's' = x ++ "'"
  | otherwise = x ++ "'s"
