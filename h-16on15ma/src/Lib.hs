module Lib where

import Helpers

import qualified Data.Map as M
import qualified Data.Maybe

import Control.Monad ( liftM2 )

{-| defines the style in which a note or chord is played
-}
data StyleModifier = Normal -- ^ just normal picking or plucking
                  | PalmMute -- ^ use palm mute
                  | Squeak   -- ^ make it squeal (usually with pick-harmonics)
     deriving (Eq, Ord, Show)

{-| defines whether and how a specific string on the guitar shall be played 
-}
data PlayedStringInfo = Unplayed -- ^ the string is not played/plucked/picked/etc.
                      | Muted    -- ^ the string is played but the fingers mute it, normally depcited by X in a tab
                      | Fret -- ^ the string is played
                         Int -- ^ indicates where the string is fretted, 0 = open string
     deriving (Eq, Ord)

instance Show PlayedStringInfo where
     show Unplayed = "-"
     show Muted = "x"
     show (Fret x) = show x

-- | abbreviation to write down an open string
openString :: PlayedStringInfo
openString = Fret 0

-- | defines the length of a note/chord
data Duration = UnpreciseDuration -- ^ no duration is given
              | Duration -- ^ note length
                      Int -- ^ numerator of note length, usually 1 
                      Int -- ^ denominator of note length, usually 8 or 16
     deriving (Eq, Ord, Show)

-- | type representing an element in a guitar tab
data TabElement = Chord {-^ strummed chord/note -}
                    [PlayedStringInfo] {-^ what to do with each string -}
                    StyleModifier {-^ how to play-}
                    Duration {-^ how long? -}
                | Rest {-^ a rest -} Duration {-^ how long? -}
                | Bar {-^ a bar -} String {-^ info what kind of bar -}
     deriving (Eq, Ord, Show)

-- | defines a (guitar) tuning (and the number of frets available)
data Tuning = Tuning [Int] -- ^ list of the pitches that correspond to the strings, lowest string goes first
                      Int  -- ^ highest fret the instrument has
     deriving (Eq, Ord, Show)

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

-- | returns candidate frettings for a given pitch
getPitchFretCandidates :: Tuning {-^ guitar tuning -} -> Int {-^ pitch -} -> [[PlayedStringInfo]]
getPitchFretCandidates (Tuning t maxFret) pitch = map playOnNthString usableStrings
     where nbrStrings = length t
           usableStrings = filter mayUseString [0..nbrStrings-1]
           mayUseString n = let f = theoreticalFret pitch n
                             in (f > 0) && (f <= maxFret)
           theoreticalFret p n = p - (t !! n)
           unplayedStrings = repeat Unplayed
           playOnNthString n = take n unplayedStrings ++ [Fret $ theoreticalFret pitch n] ++ take (nbrStrings - n - 1) unplayedStrings

-- | Tests whether two frettings are compatible
isFrettingCompatibleWith :: [PlayedStringInfo] -> [PlayedStringInfo] -> Bool
isFrettingCompatibleWith [] _ = True
isFrettingCompatibleWith _ [] = True
isFrettingCompatibleWith (Unplayed:xs) (y:ys) = isFrettingCompatibleWith xs ys
isFrettingCompatibleWith (x:xs) (Unplayed:ys) = isFrettingCompatibleWith xs ys
isFrettingCompatibleWith (x:xs) (y:ys) | x == y    = isFrettingCompatibleWith xs ys
                                       | otherwise = False

-- | this data structure encapsulates personal preferences/abilities for choosing fingerings
data FrettingPreferences = FrettingPreferences
    { isPossible :: [PlayedStringInfo] -> Bool -- ^ Given a (partial) fretting, is it at least theoretically possible to play it like this?  
    , partialPenaltyRating :: [PlayedStringInfo] -> Double -- ^ Given a (partial) freting, how ugly is it? Higher values are worse. __Must be monotonely increasing when adding fingers/frets.__
    , successionPartialPenaltyRating :: [PlayedStringInfo] -> [PlayedStringInfo] -> Double -- ^ Given two (partial) frettings, how hard/ugly is it to move from the first to the second one
    , successionMomentumPartialPenaltyRating :: [PlayedStringInfo] -> [PlayedStringInfo] -> [PlayedStringInfo] -> Double -- ^ Given three (partial) frettings, how ugly is the change in movement from the first/second to the second/third fretting
    , penaltyRating :: [[PlayedStringInfo]] -> Double -- ^ penalty for a given sequence of frettings
    }

-- | tests whether you have to put down a finger to play the string
isFretted :: PlayedStringInfo -> Bool
isFretted (Fret x) = x > 0 -- open strings are not considered fretted
isFretted _ = False

-- | Information on how to play a given string together with context information on the other strings around
data MorePlayedStringInfo = MorePlayedStringInfo
     { i_played :: PlayedStringInfo -- ^ actual info for playing this thing
     , i_open   :: Bool -- ^ is it an open string?
     , i_fretted :: Maybe Int -- ^ is it a fretted string? If yes, then where?
     , i_notPlayed :: Bool -- ^ is it supposed to be muted/unplayed?
     , i_isPlayed  :: Bool -- ^ not. i_notPlayed
     , i_sthPlayedBelow :: Bool -- ^ is there a lower string that is played, too?
     , i_sthPlayedAbove :: Bool -- ^ is there a higher string that is played, too?
     , i_fretBelow :: Maybe Int -- ^ the fret of the next-lower played string (open = 0), if this is played
     , i_fretAbove :: Maybe Int -- ^ the fret of the next-higher played string (open = 0), if this is played
     , i_nbrMutedBelow :: Maybe Int -- ^ if there are played strings below, then this is the nbr of muted strings inbetween
     , i_nbrMutedAbove :: Maybe Int -- ^ if there are played strings above, then this is the nbr of muted strings inbetween
     , i_lowestFretPlayed :: Maybe Int -- ^ this is the lowest fret that has to be played together with this (open strings do not count here)
     , i_highestFretPlayed :: Maybe Int -- ^ this is the highest fret that has to be played together with this
     } deriving (Eq, Ord, Show)

-- | compute the statistics of a chord from its components
computeMorePlayedStringInfos :: [PlayedStringInfo] -> [MorePlayedStringInfo]
computeMorePlayedStringInfos f = zipWith (curry getInfo) f [0..]
   where getInfo (x,n) = MorePlayedStringInfo
             { i_played = x
             , i_open   = isOpen x
             , i_fretted = isFretted x
             , i_notPlayed = isUnplayed x
             , i_isPlayed  = isPlayed x
             , i_sthPlayedBelow = anythingPlayed $ take n f
             , i_sthPlayedAbove = anythingPlayed $ drop (n+1) f
             , i_fretBelow = last' $ justPlayedFrets $ take n f
             , i_fretAbove = head' $ justPlayedFrets $ drop (n+1) f
             , i_nbrMutedBelow = nbrMuted $ reverse $ take n f
             , i_nbrMutedAbove = nbrMuted $ drop (n+1) f
             , i_lowestFretPlayed = lowestFret
             , i_highestFretPlayed = highestFret
             }
         justPlayedFrets x = filterAndGetFrets x
         filterAndGetFrets x = map getFret $ filter isFret x
         getFret (Fret x) = x
         isOpen (Fret 0) = True
         isOpen _ = False
         isFret (Fret _) = True
         isFret _ = False
         isFretted (Fret 0) = Nothing
         isFretted (Fret x) = Just x
         isFretted _ = Nothing
         isUnplayed Unplayed = True
         isUnplayed Muted = True
         isUnplayed _ = False
         isPlayed = not . isUnplayed
         anythingPlayed = any isPlayed
         last' [] = Nothing
         last'  x = Just $ last x
         head' [] = Nothing
         head'  x = Just $ head x
         countMutedHead [] = 0
         countMutedHead (x:xs) | isUnplayed x = 1 + countMutedHead xs
                               | otherwise    = 0
         nbrMuted x | anythingPlayed x =  Just $ countMutedHead x
                    | otherwise        = Nothing
         lowestFret  | null (justPlayedFrets f) = Nothing
                     | otherwise             = Just $ minimum $ justPlayedFrets f
         highestFret | null (justPlayedFrets f) = Nothing
                     | otherwise             = Just $ maximum $ justPlayedFrets f

-- | this function rates how unpreferred the position on the neck is
myFretPenalty :: [MorePlayedStringInfo] -> Double
myFretPenalty [] = 0
myFretPenalty frets  = penalty + highFretPenalty + mediumHighFretPenalty
  where
       playedFrets = map (getFret . i_played) (filter isPlayedFret frets)
       isPlayedFret m = Data.Maybe.isJust (i_fretted m)
       getFret (Fret x) = x
       penalty | null playedFrets = 0 -- no fretting, no penalty
               | otherwise         = rateFret $ minimum playedFrets
       rateFret 5 = 0.0
       rateFret x | x < 5 = 4.0 * (5.0 - fromIntegral x)
                  | x > 5 = 6.0 * (fromIntegral x - 5.0)
       highFretPenalty = (*) 50.0 $ fromIntegral $ length $ filter (16 <) playedFrets
       mediumHighFretPenalty = (*) 5.0 $ fromIntegral $ length $ filter (16 >) $ filter (13 <=) playedFrets

-- | this function rates how poor the open strings are
myOpenStringPenalty :: [MorePlayedStringInfo] -> Double
myOpenStringPenalty [] = 0
myOpenStringPenalty frets  = (*) 35.0 $ fromIntegral $ length $ filter i_open frets

-- | this function rates how bad muted strings lie between non-muted strings
myMutedSkippedStringPenalty :: [MorePlayedStringInfo] -> Double
myMutedSkippedStringPenalty f = skipPenalty
  where
     skipPenalty = (+) openMutePenalty $ sum $ map skipNStringsPenalty [1..length f-2]
     stringOpenTest = i_open
     stringUnplayedTest m = i_notPlayed m && i_sthPlayedAbove m
     openMutePatternTest = [stringOpenTest, stringUnplayedTest]
     skipNStringsPatternTest n m = (i_nbrMutedAbove m == Just n) && i_isPlayed m
     skipNStringsPenalty n = (*) (penaltyFactor n) $ fromIntegral $ length $ filter (skipNStringsPatternTest n) f
     penaltyFactor 1 = 100.0
     penaltyFactor _ = 110.0 -- >1 skipped string
     openMutePenalty = (*) 250.0 $ fromIntegral $ countPatternInList openMutePatternTest f

-- | this function rates how much span the fretting needs
myFrettingSpanPenalty :: [MorePlayedStringInfo] -> Double
myFrettingSpanPenalty f = maxDistancePenalty + overallDistancePenalty
  where
     pressurePoints = map toPosition2d $ filter mustPushDown $ zip f [0..]
     mustPushDown (m,_) = Data.Maybe.isJust (i_fretted m)
     toPosition2d (m,n) = let Just fret = i_fretted m
                              fr = fromIntegral fret
                              alpha = (fr - 1.0) / 23.0 -- string spacing increases towards the bridge.
                              y = (3.5 - fromIntegral n) * (0.6 + (1.0 - alpha) * 0.3) -- on an 8 string, the line between string 4 and 5 is perpendicular to the frets
                              x = sum $ map fretWidth $ take fret [1..]
                              fretWidth 1 = 0
                              fretWidth 2 = 3.75 -- 2nd fret is about 3.75cm from the first fret
                              fretWidth k = fretWidth (k-1) * semitoneStepFactor
                              semitoneStepFactor = 0.5 ** (1.0 / 12.0)
                         in (x,y)
     pairwiseDistances = map computeAllDistances pressurePoints
     computeAllDistances (x,y) = map (computeDistance (x,y)) pressurePoints
     computeDistance (x,y) (u,v) = sqrt $ (u-x)**2.0 + (v-y)**2.0
     maximumDistances = map maximum pairwiseDistances
     sumOfDistances = (*) 0.5 $ sum maximumDistances
     maxDistance = maximum maximumDistances
     penalty x | x < 9.5 = 0
               | otherwise = (x - 9.5) * 555.6
     maxDistancePenalty = penalty maxDistance
     overallDistancePenalty = sumOfDistances * 8.5

-- | this function penalizes when the fret on the lower (pitch) string is higher than the fret on the higher (pitch) string
myInverseFrettingPenalty :: [MorePlayedStringInfo] -> Double
myInverseFrettingPenalty f = (*) 40.0 $ fromIntegral $ length $ filter filterInversions1 $ filter filterInversions0 f
  where
       filterInversions0 m = isOfFretType (i_played m) && (i_nbrMutedAbove m == Just 0) && i_sthPlayedAbove m
       isOfFretType (Fret _) = True
       isOfFretType _ = False
       filterInversions1 m = getFret (i_played m) > Data.Maybe.fromMaybe 0 (i_fretAbove m)
       getFret (Fret x) = x

-- | normalizes the number of sub-list elements by adding Unplayed list elements
normalizePlayedStringInfoList :: [[PlayedStringInfo]] -> [[PlayedStringInfo]]
normalizePlayedStringInfoList [] = []
normalizePlayedStringInfoList x = normalizeTo n x
     where     n = maximum $ map length x
               normalizeTo _ [] = []
               normalizeTo n (x:xs) = fixLength n x : normalizeTo n xs
               fixLength n [] = map (const Unplayed) [1..n]
               fixLength 0 _  = []
               fixLength n (x:xs) = x : fixLength (n-1) xs


-- | my personal fretting style
myFrettingPreferences :: FrettingPreferences
myFrettingPreferences = FrettingPreferences
     { isPossible = possible
     , partialPenaltyRating = frettingPenalty
     , successionPartialPenaltyRating = frettingChangePenalty
     , successionMomentumPartialPenaltyRating = frettingChangeMomentumPenalty
     , penaltyRating = penaltyRating
     }
  where penaltyRating' [] = 0.0
        penaltyRating' [x] = frettingPenalty x
        penaltyRating' [x,y] = frettingPenalty x + frettingPenalty y + frettingChangePenalty x y
        penaltyRating' (x:(y:(z:zs))) = frettingPenalty x + frettingChangePenalty x y + frettingChangeMomentumPenalty x y z
        penaltyRating = penaltyRating' . normalizePlayedStringInfoList
        possible fretting = let playedStrings = map getFret $ filter isFretted fretting
                                is_possible [] = True
                                is_possible x = maximum x - minimum x <= 5 -- more than 5 is definitely not possible in almost all cases!
                             in is_possible playedStrings
        getFret (Fret x) = x
        frettingPenalty [] = 0.0
        frettingPenalty f = let i = computeMorePlayedStringInfos f
                                penalties = [ (*) 1.0 $ myFretPenalty i
                                            , (*) 1.0 $ myOpenStringPenalty i
                                            , (*) 1.0 $ myMutedSkippedStringPenalty i
                                            , (*) 1.0 $ myFrettingSpanPenalty i
                                            , (*) 1.0 $ myInverseFrettingPenalty i
                                            ]
                             in sum penalties
        frettingChangePenalty f0 f1 = let i0 = computeMorePlayedStringInfos f0
                                          i1 = computeMorePlayedStringInfos f1
                                          penalties = [(*) 1.0 $ myNeckAndFingerMovePenalty i0 i1,
                                                       (*) 1.0 $ myStringChangePenalty i0 i1]
                                       in sum penalties
        frettingChangeMomentumPenalty f0 f1 f2 = let i0 = computeMorePlayedStringInfos f0
                                                     i1 = computeMorePlayedStringInfos f1
                                                     i2 = computeMorePlayedStringInfos f2
                                                     penalties = [(*) 1.0 $ myFretChangeMomentumPenalty i0 i1 i2]
                                                  in sum penalties
-- | this function defines how poor the change in strings pinched down between two frettings in succession is
myStringChangePenalty :: [MorePlayedStringInfo] -> [MorePlayedStringInfo] -> Double
myStringChangePenalty [] _ = 0.0
myStringChangePenalty _ [] = 0.0
myStringChangePenalty f0 f1 = sum penalties
     where
          s0 = map isStringUsed f0
          s1 = map isStringUsed f1
          l0 = lowestStringUsed s0
          l1 = lowestStringUsed s1
          h0 = highestStringUsed s0
          h1 = highestStringUsed s1
          isStringUsed x = Data.Maybe.isJust $ i_fretted x
          lowestStringUsed' _ [] = -1
          lowestStringUsed' n (True:xs) = n
          lowestStringUsed' n (False:xs) = lowestStringUsed' (n+1) xs
          lowestStringUsed = lowestStringUsed' 0
          highestStringUsed' _ [] = -1
          highestStringUsed' n (True:xs) = max n $ highestStringUsed' (n+1) xs
          highestStringUsed' n (False:xs) = highestStringUsed' (n+1) xs
          highestStringUsed = highestStringUsed' 0
          lowRangePenalty | l0 < 0 || l1 < 0 = 0.0
                          | l1 > l0 = 0.0
                          | otherwise = 75.0 * (fromIntegral (l0 - l1) ** 1.40)
          highRangePenalty | h0 < 0 || h1 < 0 = 0.0
                           | h1 < h0 = 0.0
                           | otherwise = 75.0 * (fromIntegral (h1 - h0) ** 1.40)
          skipUnplayed [] = []
          skipUnplayed (False:xs) = skipUnplayed xs
          skipUnplayed x0@(True:_) = x0
          countNewOnsets = \x -> fromIntegral . length . filter (True ==) . zipWith isNewOnset x
          newOnsetPenalty0 = 25.0 * countNewOnsets s0 s1
          newOnsetPenalty1 = 25.0 * countNewOnsets (skipUnplayed s0) (skipUnplayed s1)
          isNewOnset False True = True
          isNewOnset _ _ = False
          newOnsetPenalty = min newOnsetPenalty0 newOnsetPenalty1
          penalties = [lowRangePenalty, highRangePenalty, newOnsetPenalty]

-- | this function defines how poor the neck move needed for two frettings in succession is
myNeckAndFingerMovePenalty :: [MorePlayedStringInfo] -> [MorePlayedStringInfo] -> Double
myNeckAndFingerMovePenalty [] _  =  0.0
myNeckAndFingerMovePenalty _ []  =  0.0
myNeckAndFingerMovePenalty f0 f1 =  minimum [absolutePenalty, relativePenalty]
     where
          getFret (Fret x) = x
          alteredFretting = zipWith fretChanges f0 f1
          fretChanges i0 i1 | isFretted (i_played i0) && isFretted (i_played i1) =
                                   Just $ getFret (i_played i1) - getFret (i_played i0)
                            | otherwise = Nothing
          alteredLowestFret = i_lowestFretPlayed (head f1) `maybeMinus` i_lowestFretPlayed (head f0)
          relativeAlteredFretting = map (`maybeMinus` alteredLowestFret) alteredFretting
          maybeMinus = liftM2 (-)
          absolutePenalty = (*) 15.0 $ sum $ map (fromIntegral . abs . Data.Maybe.fromMaybe 0) alteredFretting
          relativePenalty0 = (*) 5.0 $ sum $ map (fromIntegral . abs . Data.Maybe.fromMaybe 0) relativeAlteredFretting
          neckMovePenalty0 = (*) 8.0 $ abs $ fromIntegral $ Data.Maybe.fromMaybe 0 alteredLowestFret
          relativePenalty | Data.Maybe.isJust alteredLowestFret = relativePenalty0 + neckMovePenalty0
                          | otherwise = absolutePenalty

-- | this function defines how poor the momentum of fretting successions is
myFretChangeMomentumPenalty :: [MorePlayedStringInfo] -> [MorePlayedStringInfo] -> [MorePlayedStringInfo] -> Double
myFretChangeMomentumPenalty [] _ _ = 0.0
myFretChangeMomentumPenalty _ [] _ = 0.0
myFretChangeMomentumPenalty _ _ [] = 0.0
myFretChangeMomentumPenalty f0 f1 f2 = penalty
     where     penalty = 15.0 * secondDifference
               diff0 = maybeAbs $ i_lowestFretPlayed (head f1) `maybeMinus` i_lowestFretPlayed (head f0)
               diff1 = maybeAbs $ i_lowestFretPlayed (head f2) `maybeMinus` i_lowestFretPlayed (head f1)
               secondDifference = fromIntegral $ abs $ Data.Maybe.fromMaybe 0 (diff1 `maybeMinus` diff0)
               maybeMinus = liftM2 (-)
               maybeAbs = fmap abs


-- | check possibility of a given fretting using myFrettingPreferences
myIsPossible :: [PlayedStringInfo] -> Bool
myIsPossible = isPossible myFrettingPreferences

-- | determine rating of a given fretting using myFrettingPreferences
myPartialPenaltyRating :: [PlayedStringInfo] -> Double
myPartialPenaltyRating = partialPenaltyRating myFrettingPreferences

-- | determine my preferred rating for a given sequence of frettings
myPenaltyRating :: [[PlayedStringInfo]] -> Double
myPenaltyRating = penaltyRating myFrettingPreferences
