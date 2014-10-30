-- UPenn Course hwrk 1

-- #2 Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

--moving from left to right, middle is storage.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, c)]
hanoi 2 a b c = [(a, b), (a, c), (b, c)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) b a c)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b c d = [(a,d)]
hanoi4 2 a b c d = [(a, b), (a, d), (b, d)]
hanoi4 3 a b c d = [(a, b), (a, c), (a, d), (c, d), (b, d)]
hanoi4 n a b c d = hanoi (n-1) a c d b ++ hanoi 1 a b d c ++
-- unfinished
