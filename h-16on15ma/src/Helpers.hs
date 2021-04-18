module Helpers where

import Data.Maybe ( fromJust, isNothing, isJust )
import Data.List (sortOn)
import Control.Parallel.Strategies
import Control.Monad


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

optimizePenaltyPointWiseGreedy penalty choiceset input =
    let maybeBestChoices = [if null choices then Nothing else Just $ head choices
                              | x <- input
                              , let choices = sortOn chordPenalty $ choiceset x]
        chordPenalty x = penalty [x]
        result | any isNothing maybeBestChoices = Nothing 
               | otherwise = Just $ map fromJust maybeBestChoices
      in result


{- | Implements a heuristic for optimizing the penalty with a lookahead.

  Plan for a lookahead heuristic:

  Determine the initial bound on the lookahead window using optimizePenaltyGreedyPrefixUpperBound.

  Determine all choices for the head position, that are below the initial bound.
    
    For each of these candidate choices, determine the heuristically best solution for the rest of the window
    with a reduced lookahead size.

    Choose the best candidate from these and recurse with the same lookahead size.
-}

optimizePenalty penalty choiceset lookback lookahead input =
    let run partPrefix window further
            | isNothing partPrefix = Nothing
            | null window          = partPrefix
            | otherwise            =
                let doStep = optimizePenaltyWindowStep penalty choiceset lookback (fromJust partPrefix) window
                    fix_one = head $ fromJust doStep
                    nextPrefix = Just $ fromJust partPrefix ++ [fix_one]
                    nextWindow | null further = tail window
                               | otherwise    = tail window ++ [head further]
                    nextFurther | null further = []
                                | otherwise = tail further
                    recurse_other = run nextPrefix nextWindow nextFurther
                    stepResult | isNothing doStep = Nothing
                               | isNothing recurse_other = Nothing
                               | otherwise = recurse_other
                 in stepResult
     in run (Just []) (take lookahead input) (drop lookahead input)

--optimizePenaltyWindowStep :: (Ord a1, NFData a2, NFData a1) => ([a2] -> a1) -> (a3 -> [a2]) -> Int -> [a2] -> [a3] -> Maybe [a2]
optimizePenaltyWindowStep penalty choiceset lookBackLength lookBack window =
    let trimLookBack | length lookBack <= lookBackLength = lookBack
                     | otherwise = drop (length lookBack - lookBackLength) lookBack
        bound_x = --optimizePenaltyGreedyPrefixUpperBound Nothing penalty choiceset trimLookBack window
                  liftM2 (++) (Just trimLookBack) $ optimizePenaltyPointWiseGreedy penalty choiceset window -- a little bit faster
        bound   = penalty $ fromJust bound_x
        result | isNothing bound_x = Nothing
               | otherwise = optimizePenaltyWindowStep' bound penalty choiceset lookBackLength trimLookBack window
     in result

--optimizePenaltyWindowStep' :: (Ord a1, NFData a2, NFData a1) => a1 -> ([a2] -> a1) -> (a3 -> [a2]) -> p -> [a2] -> [a3] -> Maybe [a2]
optimizePenaltyWindowStep' _ _ _ _ _ [] = Just []
optimizePenaltyWindowStep' penaltyBound penalty choiceset lookBackLength lookBack window@(w:ws) =
    let bound_x = optimizePenaltyGreedyPrefixUpperBound (Just penaltyBound) penalty choiceset lookBack window
        bound   = penalty $ fromJust bound_x
        choices = --withStrategy (parList rpar)
                  [c | c <- choiceset w
                     , let p = penalty (lookBack ++ [c])
                     , p <= bound ]
        prj1 (x,_) = x
        prj2 (_,y) = y
        candidates = map prj2 $ sortOn prj1 candidates'
        candidates' = --withStrategy (parList rpar) 
                      [penaltyWithPrefix (c : cs) 
                                    |  c <- choices
                                    , let cs0 = optimizePenaltyWindowStep' penaltyBound penalty choiceset lookBackLength (lookBack ++ [c]) ws
                                    , isJust cs0 -- check whether there is at least a solution now that we fixed c
                                    , let cs = fromJust cs0] 
        penaltyWithPrefix cx = (penalty $ lookBack ++ cx, cx)
        result | isNothing bound_x = Nothing
               | null candidates   = Nothing
               | otherwise         = Just $ head candidates
     in result