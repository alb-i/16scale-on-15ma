module Helpers where

import Data.Maybe ( fromJust, isNothing )
import Data.List (sortOn)


{- | checks whether the head of a list fits a given pattern
-}
testHeadPattern :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Bool
testHeadPattern ptest list | length ptest > length list = False
                             | otherwise = all apply $ zip ptest list
                             where apply (f,x) = f x

-- | detemine how often a specific pattern occurs in given list
countPatternInList :: [a -> Bool] {-^ pattern tests -} -> [a] {-^ list to test -} -> Int
countPatternInList _ [] = 0
countPatternInList ptest list@(x:xs) | testHeadPattern ptest list = 1 + countPatternInList ptest xs
                                     | otherwise                      = countPatternInList ptest xs


{- | heuristic for select a good overall choices-list with respect to a penalty
     function by greedily selecting the best successive prefixes.
     
     This routine tries all possible choices until it finds an allowed solution,
     which is then returned; but no interactions between single elements of the
     list are considered, i.e. we (wrongly) assume that
     -- prop> penalty (x ++ y) == penalty x + penalty y
-}
optimizePenaltyGreedyPrefixUpperBound :: (Ord d) =>
     Maybe d         -- ^ upper bound
  -> ([a] -> d)      -- ^ penalty function that rates the overall series of inputs 
  -> (b -> [a])      -- ^ generates the choice set for a given input element
  -> [a]             -- ^ prefix choice-list
  -> [b]             -- ^ input list for which we search a good choice
  -> Maybe [a]

optimizePenaltyGreedyPrefixUpperBound _     _       _         prefix []     = Just prefix
optimizePenaltyGreedyPrefixUpperBound bound penalty choiceset prefix (x:xs) =
    let choices = [(p,c) | c <- choiceset x
                         , let p = penalty (prefix ++ [c])
                         , isNothing bound || (p <= fromJust bound) ]
     in if null choices then Nothing else
         let 
             prj1 (p,_)     = p
             sorted_choices = sortOn prj1 choices
             applied        = [optimizePenaltyGreedyPrefixUpperBound bound penalty choiceset (prefix ++ [c]) xs
                                | (_,c) <- sorted_choices]
             possibilities  = dropWhile isNothing applied
          in if null possibilities then Nothing else head possibilities


