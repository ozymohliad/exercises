module Main where

import Employee
import Data.Tree

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ f) (GL emps fun) = GL (emp : emps) (fun + f)

instance Semigroup GuestList where
    (GL emps1 fun1) <> (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1 > fun2 = gl1
    | otherwise   = gl2


-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root subf) = f root (map (treeFold f) subf)


-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp _ f) []  = (glCons boss mempty, mempty)
nextLevel boss@(Emp _ f) lst = (best_with, best_without)
    where best_with    = glCons boss $ mconcat (map snd lst)
          best_without = mconcat (map fst lst)


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


-- Exercise 5
stats :: Tree Employee -> [String]
stats tree = ("Total fun: " ++ show fun) : map empName emps
    where (GL emps fun) = maxFun tree

main :: IO ()
main = readFile "company.txt" >>= putStr . unlines . stats . read
