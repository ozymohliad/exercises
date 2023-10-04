{-# LANGUAGE FlexibleInstances #-}
module Calc where

import Parser
import qualified ExprT
import qualified StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT.ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7


-- Exercise 5
instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


-- Exercise 6
type VarLookup = M.Map String Integer -> Maybe Integer

class HasVars a where
    var :: String -> a

instance HasVars VarLookup where
    var = M.lookup

instance Expr VarLookup where
    lit = const . Just
    add a b m = (+) <$> a m <*> b m
    mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> VarLookup -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
