-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = mod x 10 : toDigitsRev (div x 10)


-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (\i x -> if even i then x else 2 * x) [0..]


-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev


-- Exercise 4
validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a,b)]
hanoi x a b c = hanoi (x-1) a c b ++ hanoi 1 a b c ++ hanoi (x-1) c b a


-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 x a b c d
    | x < 3     = hanoi x a b c
    | x == 3    = [(a,c), (a,d), (a,b), (d,b), (c,b)]
    | otherwise = hanoi4 x2 a c b d ++ hanoi x1 a b d ++ hanoi4 x2 c b a d
    where x2 = x - x1
          x1 = round . sqrt $ 2 * fromIntegral x
