-- Utily functions for dealing with notes and other stuff

module NoteUtil (
    makeWellFormed,
    makeLength,
    getLength,
    joinMoves,
    toAbsolute,
    fromAbsolute,
    addNotes,
    shiftPitchNotes,
    shiftPitchPitches,
    shiftPitch,
    
    seed,
    rand,
    randElem,
    shuffle,
    sometimesMap,
    rotate,
    rotations,
    unJust,
    
    WeightedList,
    expandWeighted,
    expandUnWeighted,
    weightedRandElem,
)
where

import Data.List
import System.Random
import System.IO.Unsafe as U

import Note

-- This function makes sure a list of notes has no overhanging notes
-- TODO: look at MIDI code and see if a [Note] needs to be well formed in order
-- to generate the right data.
-- TODO: Investigate how necessary this function actually is.
makeWellFormed :: [Note] -> [Note]
makeWellFormed xs = mwf (0 :: Duration) xs
    where mwf nt [] = if nt > 0 then [Move nt] else []
          mwf nt ((Note p v d):xs) = (Note p v d) : mwf (max nt d) xs
          mwf nt ((Move d):xs) = (Move d) : mwf (max 0 (nt-d)) xs

-- Trim a list of notes to have a specific length. Should re-name to makeLength
-- as it can also increase the length.
makeLength :: Duration -> [Note] -> [Note]
makeLength d = makeWellFormed . tn d . makeWellFormed
    where tn :: Duration -> [Note] -> [Note]
          tn 0 _ = []
          tn t [] = [Move t]
          tn t ((Move d):xs)
              | t >= d = (Move d) : tn (t-d) xs
              | t < d  = [Move t]
          tn t ((Note p v d):xs)
              | t >= d = (Note p v d) : tn t xs
              | t < d  = (Note p v t) : tn t xs
              
getLength :: [Note] -> Duration
getLength = gl (0 :: Duration) . makeWellFormed
    where gl d [] = d
          gl d ((Note p v t):xs) = gl d xs
          gl d ((Move d'):xs) = gl (d+d') xs 
          
-- Join adjacent moves together, so that there are no two moves next to each
-- other. Is this function necessary?
joinMoves :: [Note] -> [Note]
joinMoves [] = []
joinMoves ((Note p v d):xs) = (Note p v d) : joinMoves xs
joinMoves ((Move d):xs) = case xs of
                            [] -> [Move d]
                            ((Note p v d):ys) -> (Move d):(Note p v d) : joinMoves ys
                            ((Move d'):ys) -> joinMoves ((Move (d+d')):ys)
                            
-- "fromAbsolute . toAbsolute /= id", as the input may have two or more adjacent
-- moves. Also, if the input is longer than the end of its longest note,
-- then this extra move at the end is lost.
toAbsolute :: [Note] -> [(Duration, Note)]
toAbsolute = ta (0 :: Duration)
    where ta t [] = []
          ta t ((Note p v d):xs) = (t, Note p v d) : ta t xs
          ta t ((Move d):xs) = ta (t+d) xs

-- Assumes the list is already sorted by Duration
fromAbsolute :: [(Duration, Note)] -> [Note]
fromAbsolute = makeWellFormed . fa (0 :: Duration)
    where fa t [] = []
          fa t ((d, (Note p v d')):xs) = if d > t
                                         then (Move (d-t)) : (Note p v d') : fa d (xs)
                                         else (Note p v d') : fa t xs
                                         
-- Put two tracks on top of each other
addNotes :: [Note] -> [Note] -> [Note]
addNotes xs ys = makeLength (max (getLength xs) (getLength ys)) $ fromAbsolute $ Data.List.sortBy (\(x, y) (x', y') -> compare x x') $ (toAbsolute xs) ++ (toAbsolute ys)

-- Shift the pitch of a list of notes
shiftPitchNotes :: Int -> [Note] -> [Note]
shiftPitchNotes n = map (\note -> case note of
                                   (Note pitch volume duration) ->
                                       Note (shiftPitch n pitch) volume duration
                                   (Move duration) -> Move duration )

shiftPitchPitches :: Int -> [Pitch] -> [Pitch]
shiftPitchPitches n = map (shiftPitch n)

shiftPitch :: Int -> Pitch -> Pitch
shiftPitch n = midiToPitch . (+n) . pitchToMIDI

seed :: Int -> IO ()
seed x = setStdGen (mkStdGen x)

rand :: Int -> Int -> IO Int
rand x y = getStdRandom $ randomR (x, y)

randElem :: [a] -> IO a
randElem xs = do r <- rand 0 (length xs - 1)
                 return $ xs !! r

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do r <- rand 0 (length xs - 1)
                rest <- shuffle $ take r xs ++ drop (r+1) xs
                return $ xs !! r : rest
                
-- p == n gives n/10 probability of being mapped for each item in the list
sometimesMap :: Int -> (a -> a) -> [a] -> IO [a]
sometimesMap _ _ [] = return []
sometimesMap p f (x:xs) = do r <- rand 1 10
                             rest <- sometimesMap p f xs
                             if r <= p then return $ (f x) : rest else return $ x : rest

-- This would work if we imported Fractal
-- shuffle :: [a] -> IO [a]
-- shuffle = unwrap . shuffle'
    -- where shuffle' :: [a] -> [IO a]
          -- shuffle' []    = []
          -- shuffle (x:xs) = do <- r rand 0 (length xs - 1)
                                 -- return (xs !! r) : (shuffle $ take r xs ++ drop (r+1) xs)

-- Alternatives for those who don't like do notation
-- shuffle :: [a] -> IO [a]
-- shuffle [] = return []
-- shuffle xs = rand 0 (length xs - 1) >>= (\r ->
             -- (shuffle $ take r xs ++ drop (r+1) xs) >>= (\rest ->
             -- (return $ xs !! r : rest)))
             
-- shuffle :: [a] -> IO [a]
-- shuffle [] = return []
-- shuffle xs = rand 0 (length xs - 1) >>= (\r -> fmap (\rest -> xs !! r : rest) $ (shuffle $ take r xs ++ drop (r+1) xs))

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

rotations :: [a] -> [[a]]
rotations xs = rts (length xs) xs
    where rts :: Int -> [a] -> [[a]]
          rts _ [] = []
          rts 0 _ = []
          rts n xs = let y = rotate xs in y : rts (n-1) y

unJust :: Maybe a -> a
unJust (Just x) = x

type WeightedList a = [(Int, a)]

expandWeighted :: WeightedList a -> [a]
expandWeighted [] = []
expandWeighted ((n, x):xs) = take n (repeat x) ++ expandWeighted xs

-- Note that this is NOT equivalent to (nub . expandWeighted), as even items
-- with a frequency of zero are included once and this is relied upon in the
-- use of this function.
expandUnWeighted :: WeightedList a -> [a]
expandUnWeighted = map snd

weightedRandElem :: WeightedList a -> IO a
weightedRandElem = randElem . expandWeighted