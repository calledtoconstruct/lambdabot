module Lambdabot.Plugin.Haskell.Free.Test where

import Lambdabot.Plugin.Haskell.Free.Type (Type (TyArr, TyForall, TyVar))

tUndef :: [Char]
tUndef = "undefined :: a -> a"

tMzero :: [Char]
tMzero = "mzero :: [a]"

tReturnList :: [Char]
tReturnList = "return :: a -> [a]"

tHead :: [Char]
tHead = "head :: [a] -> a"

tTail :: [Char]
tTail = "tail :: [a] -> [a]"

tId :: [Char]
tId = "id :: a -> a"

tConst :: [Char]
tConst = "const :: a -> b -> a"

tIdPair :: [Char]
tIdPair = "id :: (a,b) -> (a,b)"

tSwap :: [Char]
tSwap = "swap :: (a,b) -> (b,a)"

tGenSwap :: [Char]
tGenSwap = "genSwap :: (forall z. a -> b -> z) -> (forall z. b -> a -> z)"

tMap :: [Char]
tMap = "map :: (a -> b) -> ([a] -> [b])"

tZip :: [Char]
tZip = "zip :: ([a],[b]) -> [(a,b)]"

tIdFun :: [Char]
tIdFun = "id :: (a -> b) -> (a -> b)"

tFst :: [Char]
tFst = "fst :: (a,b) -> a"

tFstFun :: [Char]
tFstFun = "fst :: (a->b,c) -> a -> b"

tSnd :: [Char]
tSnd = "snd :: (a,b) -> b"

tContinuation :: Type -> Type
tContinuation a = TyForall "R" (TyArr (TyArr a r) r)
 where
  r = TyVar "R"

tReturnC :: [Char]
tReturnC = "return :: a -> (forall r. (a -> r) -> r)"

tCallCC :: [Char]
tCallCC = "callcc :: ((a -> (forall r. (b -> r) -> r)) -> (forall r. (a -> r) -> r)) -> (forall r. (a -> r) -> r)"

tPierce :: [Char]
tPierce = "pierce :: ((a -> b) -> a) -> a"

tNot :: [Char]
tNot = "not :: (forall z. z -> z -> z) -> (forall z. z -> z -> z)"

-- vim: ts=4:sts=4:expandtab:ai
