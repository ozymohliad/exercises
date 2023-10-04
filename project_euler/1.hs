sumOfMultiplesUnder :: Integer -> Integer
sumOfMultiplesUnder n = s3 + s5 - s15
    where [s3,s5,s15] = map (sum . takeWhile (<=n) . multiples) [3,5,3*5]
          multiples x = iterate (+x) x
