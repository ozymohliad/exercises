triplets = filter (\(a, b, c) -> a^2 + b^2 == c^2) [(k,j,i) | i <- [1..], j <- [1..i-1], k <- [1..j-1]]

specialTriplet = (\(a, b, c) -> a * b * c) $ head $ filter (\(a, b, c) -> a + b + c == 1000) triplets
