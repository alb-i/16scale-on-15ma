module Lib.Tunings where

import Lib.Types


-- | standard 4 string bass tuning
bassTuning :: Tuning
bassTuning = Tuning [-8, -3, 2, 7]  20
-- | standard 5 string bass tuning
bass5Tuning :: Tuning
bass5Tuning = Tuning [-13, -8, -3, 2, 7] 24
-- | standard 6 string guitar tuning
gitTuning :: Tuning
gitTuning = Tuning [4, 9, 14, 19, 23, 28] 24
-- | tuning of my 6-string guitar
my6Tuning :: Tuning
my6Tuning = Tuning [-5, 0, 5, 10, 15, 20] 22
-- | tuning of my 7-string guitar
my7Tuning :: Tuning
my7Tuning = Tuning [-8, -3, 2, 7, 12, 17, 22] 24
-- | tuning of my 8-string guitar
my8Tuning :: Tuning
my8Tuning = Tuning [-15, -8, -3, 2, 7, 12, 17, 22] 24