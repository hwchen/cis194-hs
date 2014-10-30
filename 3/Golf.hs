-- write short functions to accomplish tasks

module Golf where 

import Data.List

-- 1. takes a list a produces a list of lists. First list is
-- full list, next list contains every second element, etc...
-- I need to map a list onto each value of the original list.

-- Question is how to make that function that maps a list
-- variable in regards to original list position. (can't happen
-- recursively?) Ah, by zipping list index

--master skip function
skips :: [a] -> [[a]]
skips xs = map (f xs) (zip [0..] xs) 
    

--the function that generates a list depending on xs and list index
--how to map this one? for shorter length. If that works,
--I can just lambda it into above?
f :: [a]-> (Int, a) -> [a]
f xs (n, m) = case dropNxs of
    [] -> []
    (a:as) -> a : f as (n,m)
    where dropNxs = drop (n) xs

----------------------------------------------------------------------
--2. local maxima 

--naive
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs) 
    | (y > z) && (y > x) = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)

----------------------------------------------------------------------
--3. Histogram 

--fix type sigs later... it's a mess of ints.
--naive - first histogram, so create a list that contains
-- the count for each index. Need data type number to count.
-- then concatenate
histogram :: [Integer] -> String
histogram xs = unlines ((lines (histDrawAll (histTuple xs))) ++ ["==========\n0123456789\n"])


-- given int and list, counts occurances in list of int and returns Int
count ::  [Integer] -> Integer -> Integer 
count xs x = toInteger (length (filter (\n -> n == x) xs))

--maxCount
maxCount :: [(Integer, Integer)] -> Integer 
maxCount xs = case unZipped of
    ([], []) -> 0
    (xs, ys) -> maximum ys
    where unZipped = unzip xs

histTuple :: [Integer] -> [(Integer, Integer)]
histTuple xs = map (\n -> (n, count xs n)) [0..9]

-- Transform histogram into strings which can be concatenated
-- For string of count 1, I need to map a " "  or "*" depending on
-- if the count for each is >= 1, same for 2 etc. Order of tuple doesn't matter

--maps and concatenates each count line. List it's being mapped to is the count
-- count goes up to maxCount
histDrawAll :: [(Integer, Integer)] -> String
histDrawAll xs = unlines (reverse (map (histDrawLine xs) [1.. (maxCount xs)]))

--maps the histTuple to a single line for the count. List it's being mapped
--to is each Integer. 
--x is Integer being counted
--n is the number of times (count)
--z is # of line being drawn
--[0..9] is the list of Integers being mapped to.
histDrawLine :: [(Integer, Integer)] -> Integer -> String
histDrawLine xs z = map (\y@(x,n) -> if n >= z then '*' else ' '  ) xs


testList = [0,0,2,2,5,5,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]
testTuple = histTuple testList
