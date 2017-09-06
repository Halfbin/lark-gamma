{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Lark.Parse (parse) where

import Lark.Token
import Lark.Parser
import Lark.AST

import Data.Maybe (fromJust)
import Data.Foldable (foldl', foldr', find, asum)
import Data.Function ((&))
import Control.Applicative
import Control.Monad (guard)

data Assoc
  = LeftAssoc | RightAssoc

data Op
  = Op
    { opSpelling :: !String
    , opPrec     :: !Int
    , opAssoc    :: !Assoc
    }

opNextPrec :: Op -> Int
opNextPrec Op{..}
  = opPrec
  + case opAssoc of
      LeftAssoc  -> 1
      RightAssoc -> 0

parseOp :: Parser Token Op
parseOp = getOp <|> pure apply where
  getOp = do
    sp <- accept >>= \case
      (TkOper s) -> pure s
      _          -> empty
    let inTab = find ((== sp) . opSpelling) tab
        defOp = Just (Op sp 9 RightAssoc)
    pure $ fromJust (inTab <|> defOp)
    where
      tab =
        [ Op "<:"  5 RightAssoc
        , Op "+"   6  LeftAssoc
        , Op "-"   6  LeftAssoc
        , Op "*"   7  LeftAssoc
        , Op "/"   7  LeftAssoc
        ]
  apply = Op "" 11 LeftAssoc

parse :: [Token] -> Maybe Module
parse = evalParser mod where
  mod = Module <$> many decl
  decl = asum [ eqn ]

  eqn = DEqn <$> bindName <*> many ident <* tok TkEqual <*> expr
  bindName = ident <|> parenOper
  parenOper = tok TkOPar *> oper <* tok TkCPar

  expr = inf 0

  prim = asum [ letin, lambda, var, num, paren ]
  letin  = ELet <$ tok TkLet <*> ident <* tok TkEqual <*> expr
                <* tok TkIn <*> expr
  var    = EVar <$> ident
  num    = EInt <$> number
  paren  = tok TkOPar *> expr <* tok TkCPar

  lambda = flip (foldr' EAbs) <$ tok TkLambda <*> some ident
                              <* tok TkArrow <*> expr

  inf p = foldl' (&) <$> prim <*> many (affix p) where
    affix p = do
      op <- parseOp
      guard $ opPrec op >= p
      rhs <- inf (opNextPrec op)
      let sp = opSpelling op
      pure $ if null sp
        then \lhs -> EApp lhs rhs
        else \lhs -> EInf sp lhs rhs

  tok k = Parser $ \case
    (h : t') | h == k -> pure (t', k)
    _ -> Nothing

  ident = Parser $ \case
    (TkIdent s : t') -> pure (t', s)
    _ -> Nothing

  number = Parser $ \case
    (TkNum s : t') -> pure (t', read s)
    _ -> Nothing

  oper = Parser $ \case
    (TkOper s : t') -> pure (t', s)
    _ -> Nothing

