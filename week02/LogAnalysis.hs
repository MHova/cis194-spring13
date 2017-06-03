{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List.Split (splitOn)

parseMessage :: String -> LogMessage
parseMessage =
    innerParse . splitOn " "
  where
    innerParse ("E":severity:timestamp:message) =
      LogMessage (Error $ read severity) (read timestamp) (unwords message)
    innerParse ("W":timestamp:message) =
      LogMessage Warning (read timestamp) (unwords message)
    innerParse ("I":timestamp:message) =
      LogMessage Info (read timestamp) (unwords message)
    innerParse unknown = (Unknown . unwords) unknown

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ timestamp _) (Node leftTree node@(LogMessage _ existingTimestamp _) rightTree)
  | timestamp <= existingTimestamp = Node (insert lm leftTree) node rightTree
  | otherwise = Node leftTree node (insert lm rightTree)
insert _ _ = error "wtf there was an unknown node in the tree"

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMessage . inOrder . build . filter severeErrors
  where
    severeErrors (LogMessage (Error severity) _ _)
      | severity >= 50 = True
      | otherwise = False
    severeErrors _ = False

    toMessage (LogMessage _ _ message) = message
    toMessage (Unknown message) = message
