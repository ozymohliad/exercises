sumSquareDiff :: [Integer] -> Integer
sumSquareDiff xs = sum xs ^ 2 - sum (map (^2) xs)
