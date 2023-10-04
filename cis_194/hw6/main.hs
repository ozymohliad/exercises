{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.List (intercalate)

-- Exercise 1
fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

fibs1 :: [Integer]
fibs1 = map fib1 [0..]


-- Exercise 2
fibs2 :: [Integer]
fibs2 = map fst $ iterate next (0,1)
    where next (a,b) = (b,a+b)


-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show xs = "[" ++ intercalate "," (map show $ take 20 $ streamToList xs) ++ "...]"


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (fromIntegral . largest2Pow) $ streamFromSeed (+1) 1
    where largest2Pow = length . takeWhile even . iterate (`div` 2) 

ruler2 :: Stream Integer
ruler2 = foldr interleave $ streamMap streamRepeat $ streamFromSeed (+1) 0
    where foldr f (Cons x xs) = x `f` foldr f xs
          interleave (Cons x xs) ys = Cons x $ interleave ys xs


-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger = flip Cons (streamRepeat 0)
    negate = streamMap negate
    (Cons a as) + (Cons b bs) = Cons (a+b) (as + bs)
    (Cons a as) * ball@(Cons b bs) = Cons (a*b) (streamMap (*a) bs + as * ball)

instance Fractional (Stream Integer) where
    (Cons a as) / ball@(Cons b bs) = let q = Cons (a `div` b) (streamMap (`div`b) $ as - q * bs) in q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
    deriving (Show)

instance Num Matrix where
    (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) = Matrix
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let (Matrix _ a _ _) = Matrix 1 1 1 0 ^ n in a
