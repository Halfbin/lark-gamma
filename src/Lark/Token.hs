
module Lark.Token where

data Token
  = TkWS
  | TkLet | TkIn | TkLambda | TkArrow | TkEqual
  | TkOPar | TkCPar
  | TkIdent String
  | TkOper  String
  | TkNum   String
  deriving (Show, Eq)

