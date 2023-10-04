{-# LANGUAGE FlexibleInstances #-}
module Main where

import Sized
import Buffer
import Editor
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m a)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ jlr = jlr
jll +++ Empty = jll
jll +++ jlr   = Append (tag jll <> tag jlr) jll jlr


-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jsize :: (Sized b, Monoid b) => JoinList b a -> Int
jsize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty            = Nothing
indexJ i _ | i < 0        = Nothing
indexJ i j | i >= jsize j = Nothing
indexJ 0 (Single _ a)     = Just a
indexJ i (Single _ _)     = Nothing
indexJ i (Append _ jll jlr)
    | i < jsize jll       = indexJ i jll
    | otherwise           = indexJ (i - jsize jlr) jlr

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty            = Empty
dropJ i j | i >= jsize j = Empty
dropJ i j | i <= 0       = j
dropJ i (Append m jll jlr)
    | i == lsize         = jlr
    | i <  lsize         = let jll' = dropJ i jll in Append (tag jll' <> tag jlr) jll' jlr
    | otherwise          = dropJ (i - lsize) jlr
    where lsize = jsize jll
dropJ _ _                = undefined

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty            = Empty
takeJ i j | i <= 0       = Empty
takeJ i j | i >= jsize j = j
takeJ i (Append m jll jlr)
    | i == lsize         = jll
    | i <  lsize         = takeJ i jll
    | otherwise          = let jlr' = takeJ (i - lsize) jlr in Append (tag jll <> tag jlr') jll jlr'
    where lsize = jsize jll
takeJ _ _                = undefined


-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str


-- Exercise 4
type JoinListBuffer = JoinList (Score, Size) String

instance Buffer JoinListBuffer where
    line       = indexJ
    value b    = let (Score n, _) = tag b in n
    numLines b = let (_, Size n) = tag b in n
    toString   = concat . jlToList
    fromString = collapse . map fromLine . lines
        where fromLine str = Single (scoreString str, Size 1) str
              collapse []  = Empty
              collapse [x] = x
              collapse xs  = collapse left +++ collapse right
                where (left,right) = splitAt (length xs `div` 2) xs
    replaceLine i str b = takeJ (i-1) b +++ fromString str +++ dropJ (i+1) b


fromList :: [String] -> JoinListBuffer
fromList = fromString . unlines

main = runEditor editor . fromList $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
