-- class 4, partial application and currying

compareWithHundred :: (Num a, Ord a) =>  a -> Ordering
compareWithHundred = compare 100

--almost got this, but forgot that it's a function of z and to put that in
--at the end. Also, type signatures are ordered so it will go x y
-- type warnings told me this!
f :: (b -> c) -> (a -> b) -> (a -> c)
f x y = \z -> x (y z)

greaterThanHundred  :: [Integer] -> [Integer]
greaterThanHundred xs = filter (\x -> x > 100) xs


aTest :: [Integer] -> Bool
aTest xs = even (length (greaterThanHundred xs))

aTest' :: [Integer] -> Bool
aTest' = even . length . greaterThanHundred
