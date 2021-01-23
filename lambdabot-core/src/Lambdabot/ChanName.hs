module Lambdabot.ChanName (ChanName, mkCN, getCN) where

import Control.Applicative (Applicative (liftA2))
import Data.Char (toLower)
import Lambdabot.Nick (Nick (..))

newtype ChanName = ChanName Nick -- always lowercase
  deriving (Eq, Ord)

mkCN :: Nick -> ChanName
mkCN = ChanName . liftA2 Nick nTag (map toLower . nName)

getCN :: ChanName -> Nick
getCN (ChanName n) = n
