-- for lecture 3

data IntList = Empty | Cons Int IntList deriving Show

--map
squareAll :: IntList -> IntList 
squareAll Empty = Empty 
squareAll (Cons x xs) = Cons (x*x)  (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs) 

square x = x * x

--filter
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList f Empty = Empty
filterIntList f (Cons x xs)
    | f x == True = Cons x  (filterIntList f xs)
    | otherwise = filterIntList f xs

isEven :: Int -> Bool
isEven x
    | x `mod` 2 == 0 = True
    | otherwise = False

main = do
    let output = filterIntList isEven exampleList
        exampleList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
    print output

-- Polymorphism

data List t = E | C t (List t)

-- wildcard example

doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [_] = 0 --works because it's wildcard for single element,
doStuff1 (x:y:xs) = x + y --not just for any list
