module Helpers where

-- | checks whether the head of a list fits a given pattern
testHeadPattern :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Bool
testHeadPattern pattern list | length pattern > length list = False
                             | otherwise = all apply $ zip pattern list
                             where apply (f,x) = f x

-- | detemine how often a specific pattern occurs in given list
countPatternInList :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Int
countPatternInList _ [] = 0
countPatternInList pattern list@(x:xs) | (testHeadPattern pattern list) = 1 + (countPatternInList pattern xs)
                                       | otherwise                      = countPatternInList pattern xs