
module Main where

import Prelude hiding (lex)

import Data.Foldable (find)
import Data.Maybe (fromJust)

import Lark.AST
import Lark.Lex
import Lark.Parse
import Lark.Infer
import Lark.Eval

load :: String -> Module
load src = fromJust $ parse =<< lex src

compile = inferUnit . fmap load

main :: IO ()
main = do
  let paths = [ "prelude.lark", "test.lark" ]
  [prelude, prog] <- compile <$> traverse readFile paths

  (DEqn _ _ entry) <- case find (eqnIs "main") prog of
    (Just e) -> pure e
    Nothing  -> fail "program has no 'main' function"

  let result = eval [] entry
  putStrLn $ show result

  where
  eqnIs n (DEqn sp [] _) = n == sp
  eqnIs _ _              = False

