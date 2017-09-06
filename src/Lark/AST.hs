{-# LANGUAGE PatternSynonyms, DeriveTraversable, DeriveFoldable, DeriveFunctor
             #-}

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures
                -Wno-missing-signatures
                -Wno-incomplete-patterns
                #-}

module Lark.AST where

--import Data.Traversable (Traversable, fmapDefault, foldMapDefault)

data ModuleF d
  = Module [d]
  deriving (Traversable, Foldable, Functor)

type Module = ModuleF Decl

data DeclF e
  = DEqn String [String] e
  deriving (Traversable, Foldable, Functor)

type Decl = DeclF Expr

data ExprF a
  = EVar_ String
  | ELet_ String a a
  | EAbs_ String a
  | EApp_ a a
  | EInt_ Integer
  | EInf_ String a a
  | EIntrin_ String
  deriving (Traversable, Foldable, Functor)

instance Show a => Show (ExprF a) where
  show (EVar_ x) = x
  show (ELet_ x b e) = "(let " ++ x ++ " = " ++ show b ++ " in " ++ show e ++ ")"
  show (EAbs_ x e) = "(\\" ++ x ++ " → " ++ show e ++ ")"
  show (EApp_ f a) = "(" ++ show f ++ " " ++ show a ++ ")"
  show (EInt_ i) = show i
  show (EInf_ o l r) = "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"
  show (EIntrin_ n) = "#" ++ show n

newtype Expr = Expr (ExprF Expr)

instance Show Expr where
  show (Expr e) = show e

--{-# COMPLETE EVar, ELet, EAbs, EApp, EInt, ENil, EInf #-}
pattern EVar x     = Expr (EVar_ x)
pattern ELet x b e = Expr (ELet_ x b e)
pattern EAbs x e   = Expr (EAbs_ x e)
pattern EApp f a   = Expr (EApp_ f a)
pattern EInt i     = Expr (EInt_ i)
pattern EInf o l r = Expr (EInf_ o l r)
pattern EIntrin n  = Expr (EIntrin_ n)

