module Helpers where

-- | checks whether the head of a list fits a given pattern
testHeadPattern :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Bool
testHeadPattern ptest list | length ptest > length list = False
                             | otherwise = all apply $ zip ptest list
                             where apply (f,x) = f x

-- | detemine how often a specific pattern occurs in given list
countPatternInList :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Int
countPatternInList _ [] = 0
countPatternInList ptest list@(x:xs) | testHeadPattern ptest list = 1 + countPatternInList ptest xs
                                     | otherwise                      = countPatternInList ptest xs