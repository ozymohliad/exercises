module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage str = case words str of
    ("I":time:rest) -> LogMessage Info (read time) (unwords rest)
    ("W":time:rest) -> LogMessage Warning (read time) (unwords rest)
    ("E":sev:time:rest) -> LogMessage (Error (read sev)) (read time) (unwords rest)
    rest -> Unknown (unwords rest)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ time _) tree = case tree of
    (Node left node@(LogMessage _ nodeTime _) right)
        | time > nodeTime -> Node left node (insert msg right)
        | otherwise       -> Node (insert msg left) node right
    _ -> Node Leaf msg Leaf
insert (Unknown _) tree = tree


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ msg : inOrder right


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter err50 . inOrder . build
    where err50 (LogMessage (Error sev) _ _)
            | sev >= 50 = True
            | otherwise = False
          err50 _ = False
          getMsg (LogMessage _ _ msg) = msg
          getMsg (Unknown msg) = msg
