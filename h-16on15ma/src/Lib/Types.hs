module Lib.Types where


{-| defines the style in which a note or chord is played
-}
data StyleModifier = Normal -- ^ just normal picking or plucking
                  | PalmMute -- ^ use palm mute
                  | Squeak   -- ^ make it squeal (usually with pick-harmonics)
     deriving (Eq, Ord, Show)

{-| defines whether and how a specific string on the guitar shall be played 
-}
data PlayedStringInfo = Unplayed -- ^ the string is not played \/ plucked \/ picked \/ etc.
                      | Muted    -- ^ the string is played but the fingers mute it, normally depcited by X in a tab
                      | Fret -- ^ the string is played
                         Int -- ^ indicates where the string is fretted, 0 = open string
     deriving (Eq, Ord)

instance Show PlayedStringInfo where
     show Unplayed = "-"
     show Muted = "x"
     show (Fret x) = show x

-- | alias\/abbreviation to write down an open string
openString :: PlayedStringInfo
openString = Fret 0


-- | defines the length of a note \/ chord
data Duration = UnpreciseDuration -- ^ no duration is given
              | Duration -- ^ note length
                      Int -- ^ numerator of note length, usually 1 
                      Int -- ^ denominator of note length, usually 8 or 16
     deriving (Eq, Ord, Show)

-- | type representing an element in a guitar tab
data TabElement = Chord {-^ strummed chord \/ note -}
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

-- | this data structure encapsulates personal preferences \/ abilities for choosing fingerings
data FrettingPreferences = FrettingPreferences
    { isPossible :: [PlayedStringInfo] -> Bool -- ^ Given a (partial) fretting, is it at least theoretically possible to play it like this?  
    , partialPenaltyRating :: [PlayedStringInfo] -> Double -- ^ Given a (partial) freting, how ugly is it? Higher values are worse. __Must be monotonely increasing when adding fingers \/ frets.__
    , successionPartialPenaltyRating :: [PlayedStringInfo] -> [PlayedStringInfo] -> Double -- ^ Given two (partial) frettings, how hard \/ ugly is it to move from the first to the second one
    , successionMomentumPartialPenaltyRating :: [PlayedStringInfo] -> [PlayedStringInfo] -> [PlayedStringInfo] -> Double -- ^ Given three (partial) frettings, how ugly is the change in movement from the first \/ second to the second \/ third fretting
    , penaltyRating :: [[PlayedStringInfo]] -> Double -- ^ penalty for a given sequence of frettings
    , penaltyLookback :: Int -- ^ how many tailing elements of @xs@ affect the penalty for @xs ++ [x]@? Negative value indicates that all elements of @xs@ have an impact on adding @x@
    }

-- | Information on how to play a given string together with context information on the other strings around
data MorePlayedStringInfo = MorePlayedStringInfo
     { i_played :: PlayedStringInfo -- ^ actual info for playing this thing
     , i_open   :: Bool -- ^ is it an open string?
     , i_fretted :: Maybe Int -- ^ is it a fretted string? If yes, then where?
     , i_notPlayed :: Bool -- ^ is it supposed to be muted \/ unplayed?
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