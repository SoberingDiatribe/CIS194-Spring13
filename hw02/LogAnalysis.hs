{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage msg =
  case msgList of
    ("I":tsp:str)     -> LogMessage Info (read tsp) (unwords str)
    ("W":tsp:str)     -> LogMessage Warning (read tsp) (unwords str)
    ("E":lvl:tsp:str) -> LogMessage (Error (read lvl)) (read tsp) (unwords str)
    _                 -> Unknown msg
  where
    msgList = words msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg msgTree
  | tsp <= curTsp = Node (insert logMsg leftTree) curLogMsg rightTree
  | otherwise = Node leftTree curLogMsg (insert logMsg rightTree)
  where
    LogMessage _ tsp _ = logMsg
    Node leftTree curLogMsg rightTree = msgTree
    LogMessage _ curTsp _ = curLogMsg

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMsg rightTree)
  = inOrder leftTree ++ [logMsg] ++ inOrder rightTree

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isRelevant

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error lvl) _ _) = lvl > 50
isRelevant _                            = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ str) = str
getMessage (Unknown str)        = str
