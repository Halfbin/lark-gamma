{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures
                -Wno-missing-signatures
                -Wno-incomplete-patterns
                #-}

module Lark.Type where

import Lark.AST

data Ty
  = TInt
  | TList Ty
  | TFun Ty Ty
  | TVar Int

data TyF a
  = TyF Ty a

type ModuleT = ModuleF DeclT

type DeclT = DeclF ExprT

--{-# COMPLETE ExprT, EVarT, ELetT, EAbsT, EAppT, EIntT, ENilT, EInfT #-}
newtype ExprT = MkExprT (TyF (ExprF ExprT))
pattern ExprT t e = MkExprT (TyF t e)

pattern EVarT t x     = ExprT t (EVar_ x)
pattern ELetT t x b e = ExprT t (ELet_ x b e)
pattern EAbsT t x e   = ExprT t (EAbs_ x e)
pattern EAppT t f a   = ExprT t (EApp_ f a)
pattern EIntT t i     = ExprT t (EInt_ i)
pattern EInfT t o l r = ExprT t (EInf_ o l r)
pattern EIntrinT t n  = ExprT t (EIntrin_ n)

instance Show ExprT where
  show (ExprT t e) = "(" ++ show e ++ " : " ++ show t ++ ")"

tyOf :: ExprT -> Ty
tyOf (ExprT t _) = t

instance Show Ty where
  show TInt = "Int"
  show (TList t) = "[" ++ show t ++ "]"
  show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TVar i) = "t" ++ show i

