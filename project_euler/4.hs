digits :: Integer -> [Integer]
digits 0 = [0]
digits n = nonZeroDigits n
    where nonZeroDigits 0 = []
          nonZeroDigits n = let (d,m) = divMod n 10 in m : nonZeroDigits d

isPalindrome :: Integer -> Bool
isPalindrome = (\x -> x == reverse x) . digits

largestProductPalindrome :: Integer -> Maybe Integer
largestProductPalindrome n = safeMax $ filter isPalindrome [i*j | i <- [from..to], j <- [from..i]]
    where from = 10 ^ (n - 1)
          to   = 10 ^ n - 1
          safeMax [] = Nothing
          safeMax xs = Just $ maximum xs
