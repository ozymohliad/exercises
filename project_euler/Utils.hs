module Utils (primes, isMultipleOf, factors, mergeSorted) where

mergeSorted :: (Ord a) => [[a]] -> [a]
mergeSorted []           = []
mergeSorted ([]:yss)     = mergeSorted yss
mergeSorted ((x:xs):yss) = x : mergeSorted (insert xs yss)
    where insert x []                 = [x]
          insert x (y:ys) | x < y     = x:y:ys
                          | otherwise = y : insert x ys

diffSorted :: (Ord a) => [a] -> [a] -> [a]
diffSorted [] _ = []
diffSorted x [] = x
diffSorted xxs@(x:xs) yys@(y:ys) | x < y     = x : diffSorted xs yys
                                 | x > y     = diffSorted xxs ys
                                 | otherwise = diffSorted xs yys

primes :: [Integer]
primes = 2 : map ((+1) . (*2)) (diffSorted [1..] $ mergeSorted [[i+j+2*i*j | j <- [i..]] | i <- [1..]])

isMultipleOf :: Integer -> Integer -> Bool
isMultipleOf x m = m `mod` x == 0

factors :: Integer -> [Integer]
factors 1 = [1]
factors x = testDivisors x primes
    where testDivisors 1 _  = []
          testDivisors n [] = [n]
          testDivisors n dds@(d:ds) | n `mod` d == 0 = d : testDivisors (n `div` d) dds
                                    | otherwise      = testDivisors n ds

