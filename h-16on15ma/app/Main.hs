module Main where

import Lib

main :: IO ()
main = do
    let x = optimizePenalty myPenaltyRating (myChordFretCandidates my8Tuning) 2 8 [[i,i+7,i+12+3] | i <- [-12..18]]
    print $ show x
    return ()
