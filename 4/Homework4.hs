-- Homework for week 4

--1. WHolemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs

-- It's a fold function, since it accumulates. For every even number
-- (filter it), mulitply (itself -2) times the accumulator.

-- ah, "initial value" is accumulator, is changed each time fn
-- is called. that's what the s is. have to look carefully at 
--fn type. foldr is (a->b->b) b [a]. So the lambda should follow
-- those types. With partial application, just operator is enough.
fun1' :: [Integer] -> Integer
fun1' = foldr (\n s -> s * (n - 2)) 1 . filter (even)

-- if even, will create a list of /2, and sum
-- otherwise, pops up to an even number and follows above process.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- use takewhile, iterate
-- interesting question .. why does takeWhile evaluate the endless iterate, but
-- filter even does not? ah, because it just keeps filtering, but doesn't stop
-- the loop.
fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (>1) . iterate (\n -> if (even n) then (n `div` 2) else (3*n+1)) 

-- 2. folding over trees to generate a balanced tree (height on both sides
-- within 1. Do I need a control statement to figure out which side?
-- Also, where to put fold... inside or outside f? f is just insertion. Takes
-- a value and a tree, and inserts. How should the insertion work? It can't
-- track node height, that's state. So it can only

-- Can't just add for node height. Should be larger two node heights
-- just incrementing will increment 1 for every node underneath,
-- not just for the longest path.

-- think through node height again. Why can't use n, instead of j and k?
-- is it because j and k will track downwards, but n will not?

data Tree a = Leaf --done
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr f Leaf 

f :: (Eq a) => a -> Tree a -> Tree a
f x Leaf = Node 0 Leaf x Leaf
f x (Node n Leaf b c)    = Node 1 (f x Leaf) b c
f x (Node n a b Leaf)    = Node 1 a b (f x Leaf)
f x (Node n a@(Node j _ _ _) b c@(Node k _ _ _))
    | j > k     = Node (j+1) a b (f x c) -- set height to greater height
    | otherwise = Node (k+1) (f x a) b c 


-- 3. xor foldr, returns True only if there's an odd number of True.
-- i could implement foldr for lenght, but what's the point?

xor' :: [Bool] -> Bool 
xor' xs 
    | odd $ length $ filter (==True) xs = True
    | otherwise = False

-- need accumulator to flip, not 'a'
-- Falses can flip back Trues. Figure out no-filter later.
xor :: [Bool] -> Bool
xor = foldr (\a b -> a && (not b)) False . filter (== True) 

length' :: [a] -> Integer
length' = foldr (\_ s -> 1 + s) 0

map' :: (a -> b ) -> [a] -> [b]
map' f = foldr (\n s -> (f n) : s) []

--myFoldl :: (a -> b -> b) -> b -> [a] -> b
--myFoldl f base xs = foldr (\g x ->  ) z

--flip = g(f x)!

--I want to feed it to evaluate x1 first. Which means feeding x1 to foldr last.
--Or if i can't feed it last, then at least have it evaluate first

-- 4. Sieve of Sundaram, i + j + 2ij up to n + 2n
-- this way seems more direct, but maybe slow because filterList is large.
-- Also, produces doubles in cartProd

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\n -> 2 * n + 1) (filter (`notElem` filterList) [1 ..limit])
    where numList = cartProd [1.. limit] [1.. limit]
          filterList = map (\(x,y) -> (x + y + (2 * x * y))) numList
          limit = 2 * n + 2

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
