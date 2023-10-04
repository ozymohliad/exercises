{-# LANGUAGE TupleSections #-}

{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char
import Control.Monad

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
    fmap f (Parser g) = Parser (fmap (first f) . g)


-- Exercise 2
instance Applicative Parser where
    pure a = Parser (Just . (a,))
    Parser f <*> Parser g = Parser func
        where func s = case f s of
                Just (f, s) -> fmap (first f) (g s)
                Nothing     -> Nothing


-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = const () <$ satisfy (== 'a') <*> satisfy (== 'b')

intPair :: Parser [Integer]
intPair = (\a _ c -> [a,c]) <$> posInt <*> satisfy (== ' ') <*> posInt


-- Exercise 4
instance Alternative Parser where
    empty = Parser (const Nothing)
    Parser f <|> Parser g = Parser $ liftA2 (<|>) f g


-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
