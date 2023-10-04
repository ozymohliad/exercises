evenFibs :: [Integer]
evenFibs = let next (a,b) = (b,4*b+a) in map fst $ iterate next (2,8)

sumOfEvenFibs :: Integer -> Integer
sumOfEvenFibs n = sum $ takeWhile (<=n) evenFibs
