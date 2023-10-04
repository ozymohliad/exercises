{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char (toLower)

-- Exercise 3
newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score = scoreLower . toLower
    where scoreLower 'a' = 1
          scoreLower 'b' = 3
          scoreLower 'c' = 3
          scoreLower 'd' = 2
          scoreLower 'e' = 1
          scoreLower 'f' = 4
          scoreLower 'g' = 2
          scoreLower 'h' = 4
          scoreLower 'i' = 1
          scoreLower 'j' = 8
          scoreLower 'k' = 5
          scoreLower 'l' = 1
          scoreLower 'm' = 3
          scoreLower 'n' = 1
          scoreLower 'o' = 1
          scoreLower 'p' = 3
          scoreLower 'q' = 10
          scoreLower 'r' = 1
          scoreLower 's' = 1
          scoreLower 't' = 1
          scoreLower 'u' = 1
          scoreLower 'v' = 4
          scoreLower 'w' = 4
          scoreLower 'x' = 8
          scoreLower 'y' = 4
          scoreLower 'z' = 10
          scoreLower _   = 0

scoreString :: String -> Score
scoreString = mconcat . map score
