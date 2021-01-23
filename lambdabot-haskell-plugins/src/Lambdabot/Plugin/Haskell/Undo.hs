{-# LANGUAGE PatternSynonyms #-}

-- Copyright (c) 2006 Spencer Janssen
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

module Lambdabot.Plugin.Haskell.Undo (undoPlugin) where

import Control.Monad (guard)
import Data.Generics (Data, everywhere, listify, mkT)
import qualified Data.Set as Set
import Lambdabot.Plugin (Command (help, process), Module (moduleCmds), command, newModule, say)
import Lambdabot.Util.Parser (withParsed)
import Language.Haskell.Exts.Simple.Syntax hiding (Module)

undoPlugin :: Module ()
undoPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "undo")
              { help = say "undo <expr>\nTranslate do notation to Monad operators."
              , process = say . transform undo
              }
          , (command "do")
              { help = say "do <expr>\nTranslate Monad operators to do notation."
              , process = say . transform do'
              }
          ]
    }

findVar :: Data a => a -> String
findVar e = head $ do
  i <- [0 ..]
  x <- ['a' .. 'z']
  let xi = x : replicate i '\''
  guard $ not $ Set.member xi s
  return xi
 where
  s = Set.fromList $ listify (const True :: String -> Bool) e

transform :: (String -> Exp -> Exp) -> String -> String
transform f = withParsed $ \e -> everywhere (mkT . f . findVar $ e) e

undo :: String -> Exp -> Exp
undo v (Do stms) = f stms
 where
  f [Qualifier e] = e
  f (Qualifier e : xs) = infixed e ">>" $ f xs
  f (LetStmt ds : xs) = Let ds $ f xs
  f (Generator p e : xs)
    | irrefutable p = infixed e ">>=" $ Lambda [p] $ f xs
    | otherwise =
      infixed e ">>=" $
        Lambda [pvar v] $
          Case
            (var v)
            [ alt p (f xs)
            , alt PWildCard $
                App
                  (var "fail")
                  (Lit $ stringL "")
            ]
   where
    alt pat x = Alt pat (UnGuardedRhs x) Nothing
  f _ = error "Undo plugin error: can't undo!"
undo v (ListComp e stms) = f stms
 where
  f [] = List [e]
  f (QualStmt (Qualifier g) : xs) = If g (f xs) nil
  f (QualStmt (LetStmt ds) : xs) = Let ds $ f xs
  f (QualStmt (Generator p l) : xs)
    | irrefutable p = concatMap' $ Lambda [p] $ f xs
    | otherwise =
      concatMap' $
        Lambda [pvar v] $
          Case
            (var v)
            [ alt p (f xs)
            , alt PWildCard nil
            ]
   where
    alt pat x = Alt pat (UnGuardedRhs x) Nothing
    concatMap' fun = App (App (var "concatMap") (Paren fun)) l
  f _ = error "Undo plugin error: can't undo!"
undo _ x = x

irrefutable :: Pat -> Bool
irrefutable (PVar _) = True
irrefutable (PIrrPat _) = True
irrefutable PWildCard = True
irrefutable (PAsPat _ p) = irrefutable p
irrefutable (PParen p) = irrefutable p
irrefutable (PTuple _box ps) = all irrefutable ps
irrefutable _ = False

infixed :: Exp -> String -> Exp -> Exp
infixed l o = InfixApp l (QVarOp $ UnQual $ Symbol o)

nil :: Exp
nil = Var list_tycon_name

var :: String -> Exp
var = Var . UnQual . Ident

pvar :: String -> Pat
pvar = PVar . Ident

do' :: String -> Exp -> Exp
do' _ (Let ds (Do s)) = Do (LetStmt ds : s)
do' v e@(InfixApp l (QVarOp (UnQual (Symbol op))) r) =
  case op of
    ">>=" -> case r of
      (Lambda [p] (Do stms)) -> Do (Generator p l : stms)
      ( Lambda
          [PVar v1]
          ( Case
              (Var (UnQual v2))
              [ Alt p (UnGuardedRhs s) Nothing
                , Alt PWildCard (UnGuardedRhs (App (Var (UnQual (Ident "fail"))) _)) Nothing
                ]
            )
        )
          | v1 == v2 -> case s of
            Do stms -> Do (Generator p l : stms)
            _ -> Do [Generator p l, Qualifier s]
      (Lambda [p] s) -> Do [Generator p l, Qualifier s]
      _ -> Do [Generator (pvar v) l, Qualifier . app r $ var v]
    ">>" -> case r of
      (Do stms) -> Do (Qualifier l : stms)
      _ -> Do [Qualifier l, Qualifier r]
    _ -> e
do' _ x = x

{- | 'app' is a smart constructor that inserts parens when the first argument
 is an infix application.
-}
app :: Exp -> Exp -> Exp
app e@InfixApp{} f = App (Paren e) f
app e f = App e f
