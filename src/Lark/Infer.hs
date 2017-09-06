{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lark.Infer (inferUnit) where

import Lark.AST
import Lark.Type
import Control.Monad.RWS
import Control.Lens

type TyCtx = [(String, Ty)]
type Union = [Ty]

data InferSt
  = InferSt
    { _freshIdx :: Int
    , _tyMap    :: Union
    , _ctx      :: TyCtx
    }
makeLenses ''InferSt

type Infer a = RWS () () InferSt a

fresh :: Infer Ty
fresh = do
  i <- freshIdx <<+= 1
  tyMap %= (++ [TVar i])
  return (TVar i)

tyBinding :: String -> Infer Ty
tyBinding x = do
  (Just t) <- asks (lookup x)
  pure t

infixl 9 !!~
(!!~) :: Int -> a -> [a] -> [a]
(i !!~ a) l = let (h, _:t) = splitAt i l in h ++ a : t

unify :: Ty -> Ty -> Infer Ty
unify (TVar i) b = do
  u <- uses tyMap (!! i)
  case u of
    (TVar j) | i == j -> do
      tyMap %= i !!~ b
      pure (TVar i)
    t -> unify t b

unify a          (TVar j)     = unify (TVar j) a
unify (TList a)  (TList b)    = TList <$> unify a b
unify (TFun a b) (TFun a' b') = TFun <$> unify a a' <*> unify b b'
unify (TInt)     (TInt)       = pure $ TInt
unify a          b            = fail $ "Can't unify\n" ++ show a ++ "\n" ++ show b

unify_ :: Ty -> Ty -> Infer ()
unify_ a b = void $ unify a b

infer :: Expr -> Infer ExprT

infer (EVar x) = EVarT <$> tyBinding x <*> pure x

infer (ELet x b e) = do
  b' <- infer b
  e' <- local ((x, tyOf b') : ) $ infer e
  pure $ ELetT (tyOf e') x b' e'

infer (EAbs x e) = do
  tx <- fresh
  e' <- local ((x, tx) :) $ infer e
  pure $ EAbsT (TFun tx (tyOf e')) x e'

infer (EApp f a) = do
  f' <- infer f
  a' <- infer a
  tr <- fresh
  unify_ (tyOf f') (TFun (tyOf a') tr)
  pure $ EAppT tr f' a'

infer (EInt i)
  = pure $ EIntT TInt i

infer (EInf o l r) = do
  to <- tyBinding o
  l' <- infer l
  r' <- infer r
  te <- fresh
  unify_ to (TFun (tyOf l') (TFun (tyOf r') te))
  pure $ EInfT te o l' r'

infer (EIntrin n) = EIntrinT <$> tyBinding n <*> pure n

resolve :: [Ty] -> ExprT -> ExprT
resolve tyMap (ExprT t e) = ExprT (canon t) (fmap (resolve tyMap) e) where
  canon (TVar i) = check (tyMap !! i) where
    check (TVar j) | i == j = TVar i
    check t                 = canon t
  canon (TList a)  = TList (canon a)
  canon (TFun a b) = TFun (canon a) (canon b)
  canon t          = t

inferDecl :: Decl -> Infer DeclT
inferDecl = traverse infer

inferModule :: Module -> Infer ModuleT
inferModule = traverse inferDecl

inferUnit :: (Traversable t) => t Module -> t ModuleT
inferUnit raws =
  let (r, s', _) = runRWS (traverse inferModule raws) [] s
  in  (fmap.fmap.fmap) (resolve (s'^.tyMap)) r
  where
  s = InferSt { _freshIdx = 0, _tyMap = [] }

