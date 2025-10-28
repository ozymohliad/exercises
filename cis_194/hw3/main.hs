module Golf where

import Data.List (transpose)
import Data.Map.Lazy (empty, insertWith, findWithDefault)

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = zipWith (const skip) xs [0..]
    where skip i = map snd $ filter fst $ zip (mask i) xs
          mask i = cycle $ replicate i False ++ [True]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x:y:z:xs) <- subLists xs, y > x, y > z]
    where subLists = takeWhile (not . null) . iterate (drop 1)

-- Exercise 3
histogram :: [Integer] -> String
histogram = unlines . reverse . transpose . border . pad . toStr . count
    where border = zipWith (\i str -> show i ++ str) [0..] . map ('=':)
          pad xs = map (take (1 + maximum (map length xs)) . (++ repeat ' ')) xs
          toStr m = map (flip replicate '*' . flip (findWithDefault 0) m) [0..9]
          count = foldr (flip (insertWith (+)) 1) empty
