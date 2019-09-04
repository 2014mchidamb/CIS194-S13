{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.Read

-- Exercise 1
isValidInt :: String -> Bool
isValidInt str =
  case (readMaybe str :: Maybe Int) of
    Nothing -> False
    Just _ -> True

parseMessageWords :: [String] -> LogMessage
parseMessageWords (x:y:z:rest)
  | x == "E" && isValidInt y && isValidInt z =
    LogMessage (Error (read y :: Int)) (read z :: Int) (unwords rest)
  | x == "I" && isValidInt y =
    LogMessage Info (read y :: Int) (unwords (z : rest))
  | x == "W" && isValidInt y =
    LogMessage Warning (read y :: Int) (unwords (z : rest))
parseMessageWords ws = Unknown (unwords ws)

parseMessage :: String -> LogMessage
parseMessage msg = parseMessageWords (words msg)

parse :: String -> [LogMessage]
parse msgs = map parseMessage (lines msgs)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t _) (Node lt rmsg@(LogMessage _ ts _) rt)
  | t < ts = Node (insert msg lt) rmsg rt
  | otherwise = Node lt rmsg (insert msg rt)
insert _ _ = Leaf

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = (inOrder lt) ++ [msg] ++ (inOrder rt)

-- Exercise 5
filterSeverity :: Int -> LogMessage -> Bool
filterSeverity x (LogMessage (Error sev) _ _) = sev >= x
filterSeverity _ _ = False

extractMessage :: LogMessage -> String
extractMessage (Unknown str) = str
extractMessage (LogMessage _ _ str) = str

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = 
        map extractMessage (filter (filterSeverity 50) (inOrder (build msgs)))
