
-- for lecture 2
data FailableDouble =
    Failure
    | OK Double
    deriving Show

ex01 = Failure
ex02 = OK 3.14

mlist = ["1","2","3","4","5"]
--z = read . head . (drop 1 mlist)
