-- Data types and functions for generating and manipulating fractal music

module Fractal (
    Rule(Rule),
    Tree(Node, Leaf),
    AbstractNote,
    AbstractNoteProfiled,
    LSystem,
    grow,
    leaves,
    Applyable,
    defaultRule,
    Unwrap,
    unwrap,
    applyRules,
    growN,
    
    generateStart,
    doBuildRules,
    buildRules,
    doBuildRulesProfiled,
    buildRulesProfiled,
    doAddProfiles,
    addProfiles,
    doBuildSimpleChords,
    buildSimpleChords,
    doBuildRotatingChords,
    buildRotatingChords,
    doBuildNotesSimple,
    doBuildNotes,
    --buildNotes,
    
    filterRhythm
) where

import Data.List
import Debug.Trace

import Note
import NoteUtil

data (Applyable a b, Eq a) => Rule a b = Rule a (WeightedList [b]) deriving Show
data Tree a = Node [Tree a] | Leaf a deriving Show

type AbstractNote = (Int, Duration)
type AbstractNoteProfiled = (AbstractNote, WeightedList Int)

class LSystem s where
    grow :: (Applyable a b) => [Rule a b] -> s a -> IO (s b)
    leaves :: s a -> [a]
    
instance LSystem [] where
    grow rs = fmap concat . unwrap . map (applyRules rs)
    
    leaves = id
    
instance LSystem Tree where
    grow rs (Node xs) = fmap Node $ unwrap $ map (grow rs) xs
    grow rs (Leaf x) = do children <- applyRules rs x
                          return $ Node $ map Leaf children
    
    leaves (Node xs) = concatMap leaves xs
    leaves (Leaf x) = [x]

class (Eq a) => Applyable a b where
    defaultRule :: a -> [b]

instance Applyable Int Int where
    defaultRule = trace "defaultRule Int Int called" (:[])

-- GHC cannot coerce a constant in the file to an Int for "Applyable Int Note",
-- but can for "Applyable Int Int". Need to use ":: [Rule Int Note]" in the right places.
instance Applyable Int Note where
    defaultRule _ = trace "defaultRule Int Note called" []

instance Applyable AbstractNote AbstractNote where
    defaultRule = trace "defaultRule AbstractNote AbstractNote called" (:[])

-- This is just a guess
instance Applyable AbstractNote Note where
    defaultRule (_, x) = trace "defaultRule AbstractNote Note called" [Move x]
    
instance Applyable AbstractNote AbstractNoteProfiled where
    defaultRule (n, d) = trace "defaultRule AbstractNote AbstractNoteProfiled called" [((n, d), [(1, n)])]
    
instance Applyable AbstractNoteProfiled AbstractNoteProfiled where
    defaultRule ((n, d), p) = trace "defaultRule AbstractNoteProfiled AbstractNoteProfiled called" [((n, d), p)]
    
instance Applyable AbstractNoteProfiled Note where
    defaultRule ((_, d), _) = trace "defaultRule AbstractNoteProfiled Note called" [Move d]

-- Note that this class is dangerous as all implementations of unwrap impose
-- an "ordering" which may not be correct.
class Unwrap u where
    unwrap :: u (IO a) -> IO (u a)

instance Unwrap [] where
    unwrap [] = return []
    unwrap (x:xs) = do first <- x
                       rest  <- unwrap xs
                       return $ first : rest
                            
instance Unwrap Tree where
    unwrap (Leaf x) = do result <- x
                         return $ Leaf result
    unwrap (Node xs) = do children <- unwrap $ map unwrap xs
                          return $ Node children

-- This code may not be used
instance Functor Tree where
    fmap f (Node xs) = Node $ map (fmap f) xs
    fmap f (Leaf x) = Leaf (f x)

applyRules :: (Applyable a b) => [Rule a b] -> a -> IO [b]
applyRules rs x = 
    case findRule x rs of
        Nothing -> return $ defaultRule x
        Just r -> chooseRule r
        where findRule :: (Applyable a b) => a -> [Rule a b] -> Maybe (Rule a b)
              findRule _ [] = Nothing
              findRule n ((Rule r p):rs) = if r == n then Just (Rule r p) else findRule n rs
              -- Randomly choose an expansion of a Rule
              chooseRule :: (Applyable a b) => Rule a b -> IO [b]
              chooseRule (Rule n xs) = weightedRandElem xs
              -- The version of chooseRule below might be faster
              -- chooseRule (Rule n ps) = fmap (choose ps) $ rand 1 ((sum . map fst) ps)
              -- choose (x:xs) p = if p <= fst x then snd x else choose xs (p - fst x)

growN :: (LSystem s, Applyable a a) => Int -> [Rule a a] -> s a -> IO (s a)
growN 0 _  xs = return xs
growN n rs xs = do growOnce <- grow rs xs
                   growN (n-1) rs growOnce

-- Given a duration and a weighted list of durations, make that duration.
-- This should be replaced with a function that generates all combinations
-- which sum to d, weights them and then picks one or returns the original if
-- there are no combinations. The smallest duration in durations must be a
-- common factor of the rest for this to work.
buildLengths :: WeightedList Duration -> Duration -> IO [Duration]
buildLengths _ 0 = return []
buildLengths durations d = do first <- randElem $ filter (<=d) $ expandWeighted durations
                              rest  <- buildLengths durations (d-first)
                              return $ first : rest

buildLengthTable :: Duration -> WeightedList Duration -> IO [(Duration, [Duration])]
buildLengthTable multiplier durations =
 unwrap $ map (\d -> do { ls <- buildLengths durations (d*multiplier); return (d, ls) })
  $ expandUnWeighted durations

transposeProfile :: Int -> Int -> WeightedList Int -> WeightedList Int
transposeProfile places notesRange profile =
    map (\(x, y) -> (x, (y+places) `mod` notesRange)) profile

allPossibleProfiles :: Int -> WeightedList (WeightedList Int) -> [WeightedList Int]
allPossibleProfiles notesRange profiles = [transposeProfile n notesRange p | n <- [0..(notesRange-1)], p <- expandUnWeighted profiles]

generateStart :: Int -> Int -> Duration -> WeightedList Duration -> Int -> IO [AbstractNote]
generateStart numberOfNotes notesRange multiplier durations n = do start <- fmap (take numberOfNotes) $ shuffle $ [0..notesRange-1]
                                                                   doBuildRules multiplier (take notesRange $ map (\x -> (1, x)) start) durations n (let d = snd $ head durations in map (\x -> (x, d)) start)

-- generateStart :: [Int] -> Duration -> IO [AbstractNote]
-- generateStart notes length = do shuffled <- shuffle notes
                                -- return $ zip notes $ repeat length

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
doBuildRules :: (LSystem s) => Duration -> WeightedList Int -> WeightedList Duration
                                -> Int -> s AbstractNote -> IO (s AbstractNote)
doBuildRules multiplier notes durations n lsystem =
    do rules <- buildRules multiplier notes durations
       growN n rules lsystem

-- This function allows us to abstract the process of building L Systems.
-- Multiplier, list of 'notes', list of note lengths to create, number of times
-- to apply generated rules, [Int] can be weighted, as can [Duration]
buildRules :: Duration -> WeightedList Int -> WeightedList Duration
               -> IO [Rule AbstractNote AbstractNote]
buildRules multiplier notes durations =
 do noteTable   <- fmap (map (\xs -> (head xs, cycle $ rotate xs))) $
                    fmap rotations $ shuffle $ expandWeighted notes
    lengthTable <- buildLengthTable multiplier durations
    return [rule n d noteTable lengthTable |
            n <- expandUnWeighted notes, d <- expandUnWeighted durations]
      where rule :: Int -> Duration -> [(Int, [Int])] -> [(Duration, [Duration])]
                     -> (Rule AbstractNote AbstractNote)
            rule n d noteTable lengthTable = let ns = unJust $ lookup n noteTable
                                                 ds = unJust $ lookup d lengthTable
                                              in Rule (n, d) [(1, zip ns ds)]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
doBuildRulesProfiled :: (LSystem s) => Duration -> Int -> Int -> WeightedList Duration
                                        -> WeightedList (WeightedList Int) -> Int
                                        -> s AbstractNoteProfiled -> IO (s AbstractNoteProfiled)
doBuildRulesProfiled multiplier inNotesRange outNotesRange durations profiles n lsystem =
 do rules <- buildRulesProfiled multiplier inNotesRange outNotesRange durations profiles
    growN n rules lsystem

buildRulesProfiled :: Duration -> Int -> Int -> WeightedList Duration
                       -> WeightedList (WeightedList Int)
                        -> IO [Rule AbstractNoteProfiled AbstractNoteProfiled]
buildRulesProfiled multiplier inNotesRange outNotesRange durations profiles =
 do lengthTable <- buildLengthTable multiplier durations
    unwrap [rule n d p lengthTable | n <- [0..(inNotesRange-1)],
            d <- expandUnWeighted durations, p <- allPossibleProfiles inNotesRange profiles]
 where rule :: Int -> Duration -> WeightedList Int -> [(Duration, [Duration])]
                -> IO (Rule AbstractNoteProfiled AbstractNoteProfiled)
       rule n d p lt = do replNotes <- fmap cycle $ shuffle $ map (`mod` outNotesRange)
                           $ expandWeighted p
                          return $ Rule ((n, d), p)
                                    [(1, zip (zip replNotes (unJust $ lookup d lt)) (repeat p))]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
doAddProfiles :: (LSystem s) => Int -> WeightedList Duration -> WeightedList (WeightedList Int)
                                 -> s AbstractNote -> IO (s AbstractNoteProfiled)
doAddProfiles notesRange durations profiles lsystem =
    do rules <- addProfiles notesRange durations profiles
       grow rules lsystem

addProfiles :: Int -> WeightedList Duration -> WeightedList (WeightedList Int)
                -> IO [Rule AbstractNote AbstractNoteProfiled]
addProfiles notesRange durations profiles =
 unwrap [rule n d | n <- [0..(notesRange-1)], d <- expandUnWeighted durations]
 where rule :: Int -> Duration -> IO (Rule AbstractNote AbstractNoteProfiled)
       rule n d = do chordProfile <- randElem $ expandWeighted profiles
                     return
                      $ Rule (n, d)
                         [(1, [((n, d), transposeProfile n notesRange chordProfile)])]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
doBuildSimpleChords :: (LSystem s) => [Pitch] -> Duration -> Int -> WeightedList Duration
                                       -> WeightedList (WeightedList Int)
                                       -> s AbstractNoteProfiled -> IO (s Note)
doBuildSimpleChords pitches multiplier notesRange durations profiles lsystem =
    do rules <- buildSimpleChords pitches multiplier notesRange durations profiles
       grow rules lsystem

buildSimpleChords :: [Pitch] -> Duration -> Int -> WeightedList Duration
                      -> WeightedList (WeightedList Int) -> IO [Rule AbstractNoteProfiled Note]
buildSimpleChords pitches multiplier notesRange durations profiles =
 do profileLookupTable <- unwrap $ map (\x -> do { p <- choosePitches x ; return (x, p) } )
                           $ allPossible
    return [rule n d p profileLookupTable
            | n <- notes, d <- expandUnWeighted durations, p <- allPossible]
 where rule :: Int -> Duration -> WeightedList Int -> [(WeightedList Int, [Pitch])]
                -> Rule AbstractNoteProfiled Note
       rule n d p table =
        Rule ((n, d), p) [(1,
                           (map (\x -> Note x defaultMIDIVolume (d*multiplier))
                              $ unJust $ lookup p table) ++ [Move (d*multiplier)])]
       choosePitches :: WeightedList Int -> IO [Pitch]
       choosePitches xs = do r <- rand 3 5
                             -- Take the 5 highest weighted pitches, and randomly choose r from
                             -- them.
                             fmap (map (\x -> unJust $ lookup x pitchMap)) $ fmap (take r)
                              $ shuffle $ map snd $ take 5
                               $ sortBy (\(x, y) (x', y') -> compare x' x) xs
       pitchMap = zip notes pitches
       notes = [0..(notesRange-1)]
       allPossible = allPossibleProfiles notesRange profiles

doBuildRotatingChords :: (LSystem s) => [Pitch] -> Duration -> Int -> WeightedList Duration
                                         -> WeightedList (WeightedList Int)
                                         -> s AbstractNoteProfiled -> IO (s Note)
doBuildRotatingChords pitches multiplier notesRange durations profiles lsystem =
    do rules <- buildRotatingChords pitches multiplier notesRange durations profiles
       grow rules lsystem

buildRotatingChords :: [Pitch] -> Duration -> Int -> WeightedList Duration
                        -> WeightedList (WeightedList Int) -> IO [Rule AbstractNoteProfiled Note]
buildRotatingChords pitches multiplier notesRange durations profiles = 
 do lengthTable <- buildLengthTable multiplier durations
    unwrap $ [rule n d p lengthTable
              | n <- notes, d <- expandUnWeighted durations, p <- allPossible]
 where rule :: Int -> Duration -> WeightedList Int -> [(Duration, [Duration])]
                -> IO (Rule AbstractNoteProfiled Note)
       rule n d p lt = do ps <- choosePitches p
                          chords <- chordRotations ps
                          notes <- return $ concat $
                           zipWith (\d ps ->
                                    map (\p -> Note p defaultMIDIVolume d) ps ++ [Move d])
                            lengths (cycle chords)
                          return $ Rule ((n, d), p) [(1, notes)]
                                where lengths = unJust $ lookup d lt
       choosePitches :: WeightedList Int -> IO [Pitch]
       choosePitches xs = do r <- rand 3 5
                             -- Take the 5 highest weighted pitches, and randomly choose r from
                             -- them.
                             fmap (map (\x -> unJust $ lookup x pitchMap)) $ fmap (take r)
                              $ shuffle $ map snd $ take 5
                               $ sortBy (\(x, y) (x', y') -> compare x' x) xs
       pitchMap = zip notes pitches
       notes = [0..(notesRange-1)]
       allPossible = allPossibleProfiles notesRange profiles

chordRotations :: [Pitch] -> IO [[Pitch]]
chordRotations pitches = shuffle $ map (shiftPitchPitches (-12)) $ cr (length pitches) pitches
 where cr 0 pitches = [pitches]
       cr n (p:ps) = let result = ps ++ shiftPitchPitches 12 [p] in [result] ++ cr (n-1) result

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- doBuildNotes :: (LSystem s) => [Pitch] -> Int -> WeightedList Duration -> WeightedList (WeightedList Int) -> s AbstractNoteProfiled -> IO (s Note)
-- doBuildNotes pitches notesRange durations profiles lsystem = let rules = buildNotes pitches notesRange durations profiles
                                                              -- in grow rules lsystem

-- buildNotes :: [Pitch] -> Int -> WeightedList Duration -> WeightedList (WeightedList Int) -> [Rule AbstractNoteProfiled Note]
-- buildNotes pitches notesRange durations profiles = [rule n d p | n <- notes, d <- expandUnWeighted durations, p <- allPossibleProfiles notesRange profiles]
    -- where rule :: Int -> Duration -> WeightedList Int -> Rule AbstractNoteProfiled Note
          -- rule n d profile = let p = unJust $ lookup n pitchMap in Rule ((n, d), profile) [(1, [Note p defaultMIDIVolume d, Move d])]
          -- pitchMap = zip notes pitches
          -- notes = [0..(notesRange-1)]


doBuildNotesSimple :: [Pitch] -> [AbstractNote] -> [Note]
doBuildNotesSimple pitches =
    concat . map (\(n, d) -> [Note (pitches !! n) defaultMIDIVolume d, Move d])

doBuildNotes :: [Pitch] -> [AbstractNoteProfiled] -> [Note]
doBuildNotes pitches = concat . fmap (\((n, d), _) -> [Note (pitches !! n) defaultMIDIVolume d, Move d])

-- Filter functions

-- Could also write a function that adds non-det rules, ie given a probability
-- of a rule being replaced and a probability of it being a note
addRests :: Int -> [Rule AbstractNoteProfiled Note] -> IO [Rule AbstractNoteProfiled Note]
addRests p rules = sometimesMap p (\(Rule ((n, d), p) _) -> Rule ((n, d), p) [(1, [Move d])]) rules

-- Re-write this to use sometimesMap
filterRhythm :: Int -> [Note] -> [Note] -> IO [Note]
filterRhythm probability referenceTrack xs =
 do filteredTrack <- filterRhythm' $ toAbsolute xs
    return $ (makeLength (getLength xs)) $ fromAbsolute $ sortBy (\(x, y) (x', y') -> compare x x') $ filteredTrack
 where filterRhythm' :: [(Duration, Note)] -> IO [(Duration, Note)]
       filterRhythm' [] = return []
       filterRhythm' ((d, x):xs) = do r <- rand 0 100
                                      rest <- filterRhythm' xs
                                      if r < probability
                                       then case getClosest d of
                                             Just d' -> return $ (d', x) : rest
                                             Nothing -> return $ (d, x) : rest
                                       else return $ (d, x) : rest
       getClosest :: Duration -> Maybe Duration
       getClosest d = case (smallestAbsolute $ map (\x -> x-d) referenceTimes) of
                       (Just x) -> (Just (d+x))
                       Nothing -> Nothing
       referenceTimes :: [Duration]
       referenceTimes = nub $ map fst $ toAbsolute referenceTrack
       smallestAbsolute :: [Duration] -> Maybe Duration
       smallestAbsolute [] = Nothing
       smallestAbsolute (x:xs) = case (smallestAbsolute xs) of
                                  Nothing -> (Just x)
                                  (Just y) -> if (abs x) < (abs y) then Just x else Just y
