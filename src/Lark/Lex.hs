
module Lark.Lex (lex) where

import Prelude hiding (lex)

import Lark.Parser
import Lark.Token

import Data.Char (isSpace, isDigit, isAlpha, isPunctuation, isSymbol)
import Data.Foldable (asum)
import Control.Applicative

lex :: String -> Maybe [Token]
lex = fmap (filter (/= TkWS) . snd) . runParser top where
  top = many $ asum [ ws, ident, op, num ]
  ws = TkWS <$ some (parseIf isSpace)
  ident = make TkIdent <$> some (parseIf isAlpha)
  op    = make TkOper  <$> some (parseIf $ \c -> isPunctuation c || isSymbol c)
  num   = TkNum        <$> some (parseIf isDigit)

  make def sp
    = case lookup sp kws of
        Just t  -> t
        Nothing -> def sp
    where kws =
            [ ("let", TkLet)
            , ("in",  TkIn)
            , ("\\",  TkLambda)
            , ("->",  TkArrow)
            , ("=",   TkEqual)
            , ("(",   TkOPar)
            , (")",   TkCPar)
            ]

