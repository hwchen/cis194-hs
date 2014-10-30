-- Homework 2 UPenn course
-- Log Analysis

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--Exercise 1

parseMessage :: String -> LogMessage
parseMessage x = case head mList of
    "I" -> LogMessage Info mtimestamp mText 
    "W" -> LogMessage Info mtimestamp mText
    "E" -> LogMessage (Error errorcode) eTimestamp eText
    _   -> Unknown (unwords mList)
    where mList= words x
          mtimestamp = read (head (drop 1 mList))
          mText = unwords (drop 2 mList)
          errorcode = read (head (drop 1 mList))
          eTimestamp = read (head (drop 2 mList))
          eText = unwords (drop 3 mList)

parse :: String -> [LogMessage]
parse z = case logLines of
    [] -> []
    (x:xs) -> (parseMessage x) : parse (unlines xs)
    where logLines = lines z 

--exercise 2 insert a message into a sorted tree
--revelation: don't insert, reconstruct the tree
-- doesn't match all patterns. Is that ok? 
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)  y = y --Unknown not inserted
insert x Leaf = Node Leaf x Leaf --base case
insert x@(LogMessage _ time _) 
       (Node nodeA nodeMessage@(LogMessage _ nodeTime _) nodeB)
            | time > nodeTime = Node nodeA nodeMessage (insert x nodeB)
            | time < nodeTime = Node (insert x nodeA) nodeMessage nodeB
insert _ y = y -- for exhaustion, is this really needed? or error?


-- exercise 3 builds a sorted MessageTree
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

--exercise 4 outputs sorted List of LogMessages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node nodeA nodeMessage nodeB) = 
    (inOrder nodeA) ++ [nodeMessage] ++ (inOrder nodeB)

--exercise 5 takes unsorted [LogMessages], returns severity greater than 50
--sorted by timestamp
-- sortedList runs everytime, key was to build a filter to prevent building
-- entire logList tree. Now, each time whatWentWrong is called, only
-- takes a list of Errors.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = case sortedList of
    ((LogMessage (Error x) _ text) : xs)
        | x >= 50 -> text : whatWentWrong xs
        | otherwise -> whatWentWrong xs
    (_:xs) -> whatWentWrong xs
    [] -> []
    where sortedList = inOrder (build [a | a@(LogMessage (Error e) _ _) <-logList, e >50])

