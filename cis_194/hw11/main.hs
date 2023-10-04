{- CIS 194 HW 11
   due Monday, 8 April
-}

module Main where

import AParser
import Control.Applicative
import Data.Char
import Data.Functor
import Data.List(intercalate)
import System.Environment(getArgs)
import Data.Maybe(fromMaybe)
import Control.Monad((>=>))

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]

instance Show Atom where
    show (N int) = show int
    show (I str) = str

instance Show SExpr where
    show (A atom)  = show atom
    show (Comb xs) = "(" ++ unwords (map show xs) ++ ")"

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseComb :: Parser [SExpr]
parseComb = char '(' *> oneOrMore parseSExpr <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> (Comb <$> parseComb)) <* spaces


-- JSON
type Key = String

data KeyVal = KeyVal Key JsonObj

data JsonObj = S String
             | X Integer
             | B Bool
             | L [JsonObj]
             | J [KeyVal]

instance Show KeyVal where
    show (KeyVal k v) = "\ESC[92;1m" ++ show k ++ "\ESC[0m: " ++ show v

instance Show JsonObj where
    show (S str)  = "\ESC[94;1m" ++ show str  ++ "\ESC[0m"
    show (X int)  = "\ESC[94;1m" ++ show int  ++ "\ESC[0m"
    show (B bool) = "\ESC[94;1m" ++ show bool ++ "\ESC[0m"
    show (L obj)  = "[" ++ intercalate ", " (map show obj) ++ "]"
    show (J obj)  = "{\n" ++ intercalate ",\n" (map (indent 4 . show) obj) ++ "\n}"

indent :: Int -> String -> String
indent n = intercalate "\n" . map (replicate n ' ' ++) . lines

parseKey :: Parser Key
parseKey = char '"' *> oneOrMore (satisfy (not . ((||) <$> isSpace <*> (=='"')))) <* char '"'

parseStr :: Parser String
parseStr = char '"' *> zeroOrMore (satisfy (/= '"')) <* char '"'

parseBool :: Parser Bool
parseBool = (matchStr "true" $> True) <|> (matchStr "false" $> False)
    where matchStr = foldr ((<*>) . fmap (:) . char) (pure [])

parseArray :: Parser [JsonObj]
parseArray = char '[' *> ((++) <$> zeroOrMore (parseJsonObj <* char ',') <*> ((:[]) <$> parseJsonObj)) <* char ']'

parseKeyVal :: Parser KeyVal
parseKeyVal = KeyVal <$> (spaces *> parseKey <* char ':' <* spaces) <*> (parseJsonObj <* spaces)

parseKeyVals :: Parser [KeyVal]
parseKeyVals = char '{' *> ((++) <$> zeroOrMore (parseKeyVal <* char ',') <*> ((:[]) <$> parseKeyVal)) <* char '}'

parseJsonObj :: Parser JsonObj
parseJsonObj = trim $ (S <$> parseStr) <|> (X <$> posInt) <|> (B <$> parseBool) <|> (J <$> parseKeyVals) <|> (L <$> parseArray)
    where trim p = spaces *> p <* spaces


safeHead (x:xs) = Just x
safeHead [] = Nothing

main :: IO ()
main = do args <- getArgs
          let action = fmap (readFile >=> putStr . maybe "\ESC[91;1mError:\ESC[0m invalid JSON" (show . fst) . runParser parseJsonObj) (safeHead args)
          fromMaybe (putStr "\ESC[91;1mError:\ESC[0m no filename given") action
          putStr "\n"
