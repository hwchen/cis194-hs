-- Fibonacci

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib (x-1)) + (fib (x-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer] -- (don't use tail? I didn't figure this one out)
fibs2 = 0: 1 : zipWith (+) fibs2 (tail fibs2)

--Figuring out crazy algorithms is not my strong suite. Especially when it
-- feels like there really isn't enough background given.

map' f = foldr (\n s -> (f n) : s) []


-- part 2
-- exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show xs= showList (take 64 $ streamToList xs) "..." 

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

--exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons  (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x

--exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 1

--incorrect, the period between 2^n should continue to widen. needs to be recursive
ruler' :: Stream Integer
ruler' = interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (interleaveStreams (streamRepeat 3) (streamFromSeed (+1) 4))))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = streamLoop 0 
    where streamLoop x = interleaveStreams (streamRepeat x) (streamLoop (x+1))


