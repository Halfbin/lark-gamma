
module Lark.Parser where

import Control.Applicative
import Control.Arrow (second)
import Data.List (stripPrefix, uncons)
import Control.Monad (guard)
import Data.Tuple (swap)

newtype Parser s a
  = Parser
    { runParser :: [s] -> Maybe ([s], a)
    }

evalParser :: Parser s a -> [s] -> Maybe a
evalParser p s = snd <$> runParser p s

instance Functor (Parser s) where
  fmap f p = Parser $ fmap (second f) . runParser p

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (s, x)
  p <*> q = Parser $ \s -> do
    (t, f) <- runParser p s
    (u, x) <- runParser q t
    pure (u, f x)

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s

instance Monad (Parser s) where
  p >>= f = Parser $ \s -> do
    (t, x) <- runParser p s
    runParser (f x) t

parseIf :: (s -> Bool) -> Parser s s
parseIf p = Parser $ \s -> do
  (h, t) <- uncons s
  guard (p h)
  pure (t, h)

lit :: (Eq s) => [s] -> Parser s [s]
lit l = Parser $ \s -> do
  t <- stripPrefix l s
  pure (t, l)

accept :: Parser s s
accept = Parser $ fmap swap . uncons

