module Lambdabot.Plugin.Reference (
  dictPlugin,
  metarPlugin,
  oeisPlugin,
  searchPlugin,
  spellPlugin,
  tickerPlugin,
  urlPlugin,
  wherePlugin,
  referencePlugins,
  module Lambdabot.Config.Reference,
) where

import Lambdabot.Config.Reference (aspellBinary, proxy)
import Lambdabot.Plugin.Reference.Dict (dictPlugin)
import Lambdabot.Plugin.Reference.Metar (metarPlugin)
import Lambdabot.Plugin.Reference.OEIS (oeisPlugin)
import Lambdabot.Plugin.Reference.Search (searchPlugin)
import Lambdabot.Plugin.Reference.Spell (spellPlugin)
import Lambdabot.Plugin.Reference.Ticker (tickerPlugin)
import Lambdabot.Plugin.Reference.Url (urlPlugin)
import Lambdabot.Plugin.Reference.Where (wherePlugin)

referencePlugins :: [String]
referencePlugins = ["dict", "metar", "oeis", "search", "spell", "ticker", "url", "where"]
