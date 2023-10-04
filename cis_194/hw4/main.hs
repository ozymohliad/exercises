import Data.List ((\\))

-- Exercise 1
---- 1
fun1 :: [Integer] -> Integer
fun1 = product . map ((-)2) . filter even
---- 2
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate next
    where next n
            | even n    = n `div` 2
            | otherwise = 3 * n + 1


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node d l x r) = Node (1 + maximum (map depth [left, right])) left x right
    where (left,right) = if lnc < rnc then (insert a l, r) else (l, insert a r)
          [lnc, rnc] = map nodeCount [l, r]

nodeCount :: Tree a -> Integer
nodeCount Leaf = 0
nodeCount (Node _ l _ r) = 1 + nodeCount l + nodeCount r

depth :: Tree a -> Integer
depth Leaf = 0
depth (Node d _ _ _) = d


-- Exercie 3
---- 1
xor :: [Bool] -> Bool
xor = foldl (/=) False
---- 2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
---- 3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exerice 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . filterNums . enumFromTo 1
    where filterNums lst = lst \\ [i+j+2*i*j | i <- lst, j <- lst, i <= j]
