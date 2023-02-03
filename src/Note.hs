-- Data types for representing musical notes, and functions for writing a MIDI
-- file

module Note (
    module Data.Ratio,
    Track(Track, DelayTrack),
    Note(Move, Note),
    Pitch(Pitch),
    PitchName(Bs, C, Cs, Db, D, Ds, Eb, E, Es, Fb, F, Fs,
              Gb, G, Gs, Ab, A, As, Bb, B, Cb),
    Duration,
    defaultMIDIVolume,
    
    pitchToMIDI,
    midiToPitch,
    
    writeMIDIFile,
    giveMIDIEvents
) 
where

import Data.Ratio
import Data.Word
import Data.Bits
import Data.List
import qualified Data.ByteString as B

-- The first DeltaTime is delay at the start of the track
data Track = Track Channel [Note] | DelayTrack DeltaTime DelayProfile Channel [Note]
              deriving Show

-- For harmonic delay (swing delay)
type DelayProfile = [(Duration, DeltaTime)]

data Note = Move Duration | Note Pitch Volume Duration deriving (Eq, Show)

-- We can add "| DrumPitch PitchName" and add a whole load of drum sound names
-- to PitchName, then change pitchToMIDINum accordingly for the channel 9 drum
-- sounds as well as Eq instances.
data Pitch = Pitch PitchName Octave deriving (Eq, Show)

-- derive Eq
data PitchName = Bs | C  | Cs | Db | D  | Ds | Eb | E  | Es | Fb | F  | Fs
               | Gb | G  | Gs | Ab | A  | As | Bb | B  | Cb
                 deriving (Eq, Show)


-- This is data type used during the process of converting a list of Notes to
-- MIDI data. Bool is whether it is an on or off event, Word32 is the pitch,
-- volume is the volume, absolute time is the number of MIDI ticks since the
-- beginning of this track that this event happens at, DeltaTime is the MIDI
-- delta time for this event.
data MIDIEvent = MIDIEvent Bool MIDIPitch Volume DeltaTime deriving Show

-- In [-2, 8], see comments for pitchToMIDINum
type Octave = Int

-- In [0, 127]
type Volume = Int

-- A note length. 1/4 is a quarter note.
type Duration = Ratio Int

-- Usually in [0, 15]. Channel 9 is always drums regardless of program.
type Channel = Int

-- We could have a new data type and add names to all the instruments.
type Program = Int

-- Not sure of the range of these values, probably [40, 250]
type BPM = Int

-- In [0, 127]
type MIDIPitch = Int

type AbsoluteTime = DeltaTime
type DeltaTime = Int


-- This is an important magic number
ticksPerQuarter = 960 :: DeltaTime

defaultMIDIVolume = 80 :: Volume

-- the MIDI standard simply says that note 60 must be middle C, and since
-- there are 127 notes then we have 10+(2/3) octaves. We can choose how
-- octaves are numbered. We will say that octaves are in [0, 10].
-- The 8th Octave only has 8 notes (total range C0 to G10). C5 is middle C.
pitchToMIDI :: Pitch -> MIDIPitch
pitchToMIDI (Pitch n oc) = 12*oc + case n of
                                    Bs ->  0; C ->  0; Cs ->  1;
                                    Db ->  1; D ->  2; Ds ->  3;
                                    Eb ->  3; E ->  4; Es ->  5;
                                    Fb ->  4; F ->  5; Fs ->  6;
                                    Gb ->  6; G ->  7; Gs ->  8;
                                    Ab ->  8; A ->  9; As -> 10;
                                    Bb -> 10; B -> 11; Cb -> 11;

-- pitchToMIDINum . MIDINumToPitch /= id as, for example, we would not know
-- whether note 61 is a Cs or a Db.
-- TODO: Is this function used?
midiToPitch :: MIDIPitch -> Pitch
midiToPitch n = Pitch p oc
    where p = case n `mod` 12 of
                0  ->  C; 1  -> Cs; 2  ->  D; 3  -> Ds; 4  ->  E; 5  ->  F;
                6  -> Fs; 7  ->  G; 8  -> Gs; 9  ->  A; 10 -> As; 11 ->  B;
          oc = n `div` 12


-- The code for MIDI file generation starts here (DO NOT READ)

writeMIDIFile :: FilePath -> [Track] -> [(Channel, Program)] -> BPM -> IO ()
writeMIDIFile fileName tracks channelMap bpm = B.writeFile fileName $ B.pack $ giveFileData tracks channelMap bpm

giveFileData :: [Track] -> [(Channel, Program)] -> BPM -> [Word8]
giveFileData tracks channelMap bpm = fileHeader ++ controlTrackData ++ concatMap trackData tracks
    where fileHeader = [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x01,
                        f $ shiftR (length tracks + 1) 8,
                        f $ (length tracks + 1),
                        f $ shiftR ticksPerQuarter 8,
                        f $ ticksPerQuarter]
          -- Generate a valid track from controlTrackEventData
          controlTrackData = [0x4D, 0x54, 0x72, 0x6B]
                              ++ (intToBytes . (+4) . length) controlTrackEventData
                              ++ controlTrackEventData ++ [0x00, 0xFF, 0x2F, 0x00]
          -- Set the BPM
          controlTrackEventData = [0x00, 0xFF, 0x51, 0x03,
                                   f $ shiftR millisecondsPerQuarter 16,
                                   f $ shiftR millisecondsPerQuarter 8,
                                   f $ millisecondsPerQuarter]
          millisecondsPerQuarter = 60000000 `div` bpm
          trackData track = giveTrackData track (lookup (getChannel track) channelMap)
          f = fromIntegral

giveTrackData :: Track -> Maybe Program -> [Word8]
giveTrackData track mprogram = [0x4D, 0x54, 0x72, 0x6B] ++
                               (intToBytes . (+4) . length) trackEventData ++
                               trackEventData ++ [0x00, 0xFF, 0x2F, 0x00]
    where trackEventData = setProgramEvent ++ concatMap (midiEventData channel) (giveMIDIEvents track)
          setProgramEvent = case mprogram of
                             Nothing -> []
                             Just program -> [0x00, 0xC0 .|. f channel, f program]
          channel = getChannel track
          f = fromIntegral
                     
getChannel :: Track -> Channel
getChannel track = case track of
                    (Track channel _) -> channel
                    (DelayTrack _ _ channel _) -> channel

-- Calculate the number of MIDI ticks for a given duration. Uses the value
-- ticksPerQuarter which is defined at the start of this file.
durationToMIDITicks :: Duration -> DeltaTime
durationToMIDITicks duration = (ticksPerQuarter * 4 * (numerator duration)) `div` denominator duration

intToBytes :: Int -> [Word8]
intToBytes n = [f $ shiftR n 24, f $ shiftR n 16, f $ shiftR n 8, f n]
    where f = fromIntegral

-- Give the MIDI event data for a single MIDI event
midiEventData :: Channel -> MIDIEvent -> [Word8]
midiEventData channel (MIDIEvent state pitch volume deltaTime) = 
    giveDeltaTime deltaTime ++ [onOrOff state, f pitch, f volume]
    where onOrOff True = f (0x00000090 .|. channel)
          onOrOff False = f (0x00000080 .|. channel)
          f = fromIntegral

-- This function should error if the time is > 268435455, the largest 28 bit
-- unsigned number
giveDeltaTime :: DeltaTime -> [Word8]
giveDeltaTime deltaTime = reverse $ f (deltaTime .&. 0x0000007F) : calculateDeltaTime (shiftR deltaTime 7)
    where calculateDeltaTime 0 = []
          calculateDeltaTime time = f (time .|. 0x00000080) : calculateDeltaTime (shiftR time 7)
          f = fromIntegral

-- the equation for this is
-- (1/2)*max_delay*cos((current_location mod duration)*2pi / duration) + (1/2),
-- summed for each period of time that is swung, the derivation of which is on
-- a piece of paper somewhere. This code should be somewhere else.
-- pocket :: AbsoluteTime -> AbsoluteTime
-- pocket c = c + (round (quarterDelay * (0.5 + ((-0.5) * cos ((2*pi*((fromIntegral (c `mod` (tpq*2))) :: Double)) / ((fromIntegral (tpq*2)) :: Double))))))
                -- + (round (eighthDelay * (0.5 + ((-0.5) * cos ((2*pi*((fromIntegral (c `mod` (tpq))) :: Double)) / ((fromIntegral (tpq)) :: Double))))))
    -- where quarterDelay = 20 :: Double
          -- eighthDelay = 20  :: Double
          -- tpq = fromIntegral ticksPerQuarter :: AbsoluteTime

pocket :: DelayProfile -> AbsoluteTime -> AbsoluteTime
pocket [] currentTime = currentTime
pocket ((period, maxDelay):profiles) currentTime =
 (+ pocket profiles currentTime) $ r $ (*(f maxDelay :: Double)) $ (+0.5) $ (*(-0.5)) $
  cos $ (2*pi * f (currentTime `mod` periodTicks) :: Double) / (f periodTicks :: Double)
 where periodTicks = durationToMIDITicks period
       f = fromIntegral
       r = round

-- Generate [MIDIEvent] for a single track
giveMIDIEvents :: Track -> [MIDIEvent]
giveMIDIEvents track =
 case track of
  (Track _ notes) -> setDeltaTimes $ giveAbsoluteEvents 0 notes
  (DelayTrack startDelay delayProfile channel notes) ->
   delayHead startDelay $ setDeltaTimes $ map (\(at, me) -> (pocket delayProfile at, me)) $ giveAbsoluteEvents 0 notes

-- Take a list of MIDIEvents with well formed delta times, and apply a delay
-- in ticks by increasing the delta time of the first event.
delayHead :: DeltaTime -> [MIDIEvent] -> [MIDIEvent]
delayHead _ [] = []
delayHead startDelay ((MIDIEvent s p v deltaTime):events) = (MIDIEvent s p v (deltaTime+startDelay)) : events

-- Add delta times
setDeltaTimes :: [(AbsoluteTime, MIDIEvent)] -> [MIDIEvent]
setDeltaTimes = setDeltaTimes' 0 . sortBy (\(x, _) (y, _) -> compare x y)
    where setDeltaTimes' :: AbsoluteTime -> [(AbsoluteTime, MIDIEvent)] -> [MIDIEvent]
          setDeltaTimes' _ [] = []
          setDeltaTimes' currentTime ((absoluteTime, MIDIEvent s p v _):events) =
           if currentTime == absoluteTime
            then (MIDIEvent s p v 0) : setDeltaTimes' currentTime events
            else (MIDIEvent s p v (absoluteTime - currentTime)) : setDeltaTimes' absoluteTime events

-- Generate MIDI events with absolute times and with all delta times set to zero
giveAbsoluteEvents :: AbsoluteTime -> [Note] -> [(AbsoluteTime, MIDIEvent)]
giveAbsoluteEvents _ [] = []
giveAbsoluteEvents currentTime ((Note pitch volume duration):notes) =
 (currentTime, (MIDIEvent True midiPitch volume 0)) :
  (offNoteTime, (MIDIEvent False midiPitch volume 0)) :
   giveAbsoluteEvents currentTime notes
 where midiPitch = pitchToMIDI pitch
       offNoteTime = currentTime + durationToMIDITicks duration
giveAbsoluteEvents currentTime ((Move duration):notes) =
    giveAbsoluteEvents (currentTime + durationToMIDITicks duration) notes
