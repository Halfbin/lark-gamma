{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lark.Eval where

import Lark.Type

data Val
  = VInt Integer
  | VFun (Val -> Val)
  | VList [Val]
  | VIntrin (ValCtx -> Val)

type ValCtx = [(String, Val)]

instance Show Val where
  show (VInt i) = show i
  show (VFun _) = "<λ>"
  show (VList l) = show l
  show (VIntrin _) = "<intrinsic>"

eval :: ValCtx -> ExprT -> Val
eval ctx (EVarT    _ x)     = let (Just v) = lookup x ctx
                              in v
eval ctx (ELetT    _ x b e) = let v = eval ctx b
                              in eval ((x, v) : ctx) e
eval ctx (EAbsT    _ x e)   = VFun $ \v -> eval ((x, v) : ctx) e
eval ctx (EAppT    _ f a)   = let (VFun g) = eval ctx f
                              in g (eval ctx a)
eval _   (EIntT    _ i)     = VInt i
eval ctx (EInfT    _ o l r) = let (Just (VFun g)) = lookup o ctx
                                  (VFun h) = g (eval ctx l)
                              in  h (eval ctx r)
eval ctx (EIntrinT _ n)     = let (Just (VIntrin f)) = lookup n ctx
                              in f ctx

