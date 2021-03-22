module Lib where

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
     deriving (Eq, Ord, Show)

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
     
-- | defines a (guitar) tuning
data Tuning = Tuning [Int] -- ^ list of the pitches that correspond to the strings, lowest string goes first
     deriving (Eq, Ord, Show)
     
-- | normalizes a sequence of tab elements to fit a guitar tuning
normalizeTab :: Tuning -> [TabElement] -> [TabElement]
normalizeTab _ [] = []
normalizeTab t0@(Tuning t) (x:xs) = x_normalized : (normalizeTab t0 xs)
          where x_normalized = normalize x
                normalize (Chord f m d) = 
                    let fix_f = take nbr_strings $ f ++ repeat Unplayed
                     in Chord fix_f m d
                normalize x@_ = x
                nbr_strings = length t

-- | standard 4 string bass tuning
bassTuning = Tuning [-8, -3, 2, 7]
-- | standard 5 string bass tuning
bass5Tuning = Tuning [-13, -8, -3, 2, 7]
-- | standard 6 string guitar tuning
gitTuning = Tuning [4, 9, 14, 19, 23, 28]
-- | tuning of my 6-string guitar
my6Tuning = Tuning [-5, 0, 5, 10, 15, 20]
-- | tuning of my 7-string guitar
my7Tuning = Tuning [-8, -3, 2, 7, 12, 17, 22]
-- | tuning of my 8-string guitar
my8Tuning = Tuning [-15, -8, -3, 2, 7, 12, 17, 22]

-- | this data structure encapsulates personal preferences/abilities for choosing fingerings
data FrettingPreferences = FrettingPreferences 
    { isPossible :: [PlayedStringInfo] -> Bool -- ^ Given a (partial) fretting, is it at least theoretically possible to play it like this?  
    , partialPenaltyRating :: [PlayedStringInfo] -> Double -- ^ Given a (partial) freting, how ugly is it? Higher values are worse. __Must be monotonely increasing when adding fingers/frets.__
    }