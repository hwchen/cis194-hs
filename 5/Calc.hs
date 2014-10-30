{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- Calculator

import ExprT 
import Parser (parseExp)
import qualified StackVM as VM

--exercise 3: typeclasses

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit a = Lit a
    add x y = Add x y
    mul x y = Mul x y 

-- exercise 4: more instances

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x | x <= 0 = False | otherwise = True
    add x y = x || y
    mul x y = x && y 

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

-- Exercise 5 (stack)

instance Expr VM.Program where
    lit a = [VM.PushI a]
    add x y = (x ++ y) ++ [VM.Add]
    mul x y = (x ++ y) ++ [VM.Mul]
--    lit (Bool a)    = VM.PushB a
--    add (VM.BVal _) (VM.BVal _) = VM.And
--    mul (VM.BVal _) (VM.BVal _) = VM.Or

compile :: String -> Maybe VM.Program
compile str = parseExp lit add mul str

runStack :: Maybe VM.Program -> Either String VM.StackVal
runStack (Just xs) = VM.stackVM xs
runStack Nothing = Left $ "Error"



-- exercise 1: evaluate addition and multiplication

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

-- exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parsedString of 
    Just s -> Just $ eval s
    Nothing -> Nothing
    where parsedString = parseExp Lit Add Mul str


--Exercise 4 finish
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMinMax = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7


