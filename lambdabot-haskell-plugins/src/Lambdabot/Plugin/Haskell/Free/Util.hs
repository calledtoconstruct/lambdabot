module Lambdabot.Plugin.Haskell.Free.Util (
  Pretty (..),
  prettyParen,
  prettyParenIndent,
  module Text.PrettyPrint.HughesPJ,
) where

import Text.PrettyPrint.HughesPJ (
  Doc,
  Mode (..),
  Style (..),
  TextDetails (..),
  braces,
  brackets,
  cat,
  char,
  colon,
  comma,
  double,
  doubleQuotes,
  empty,
  equals,
  fcat,
  first,
  float,
  fsep,
  fullRender,
  hang,
  hcat,
  hsep,
  int,
  integer,
  isEmpty,
  lbrace,
  lbrack,
  lparen,
  maybeBraces,
  maybeBrackets,
  maybeDoubleQuotes,
  maybeParens,
  maybeQuotes,
  nest,
  parens,
  ptext,
  punctuate,
  quotes,
  rational,
  rbrace,
  rbrack,
  reduceDoc,
  render,
  renderStyle,
  rparen,
  semi,
  sep,
  sizedText,
  space,
  style,
  text,
  vcat,
  zeroWidthText,
  ($$),
  ($+$),
  (<+>),
  (<>),
 )

class Pretty a where
  prettyP :: Int -> a -> Doc

  pretty :: a -> Doc
  pretty x = prettyP 0 x

prettyParen :: Bool -> Doc -> Doc
prettyParen b doc = if b then parens doc else doc

prettyParenIndent :: Bool -> Doc -> Doc
prettyParenIndent b doc =
  if b
    then vcat [lparen, nest 2 doc, rparen]
    else doc

-- vim: ts=4:sts=4:expandtab
