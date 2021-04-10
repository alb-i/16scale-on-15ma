module Lib.Tabs where

import Lib.Types

-- | normalizes a sequence of tab elements to fit a guitar tuning
normalizeTab :: Tuning -> [TabElement] -> [TabElement]
normalizeTab _ [] = []
normalizeTab t0@(Tuning t _) (x:xs) = x_normalized : normalizeTab t0 xs
          where x_normalized = normalize x
                normalize (Chord f m d) =
                    let fix_f = take nbr_strings $ f ++ repeat Unplayed
                     in Chord fix_f m d
                normalize x = x
                nbr_strings = length t