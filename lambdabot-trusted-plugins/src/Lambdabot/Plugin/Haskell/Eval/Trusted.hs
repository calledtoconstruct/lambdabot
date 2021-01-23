{-# LANGUAGE Trustworthy #-}

module Lambdabot.Plugin.Haskell.Eval.Trusted (
  module Lambdabot.Plugin.Haskell.Check.ShowQ,
  module Lambdabot.Plugin.Haskell.Eval.Trusted,
) where

import Lambdabot.Plugin.Haskell.Check.ShowQ (myquickcheck)
import Math.OEIS (OEISSequence (description), SequenceData, lookupSequence)

describeSequence :: SequenceData -> Maybe String
describeSequence = fmap description . lookupSequence

newtype Mu f = In {out :: f (Mu f)}

newtype Rec a = InR {outR :: Rec a -> a}
