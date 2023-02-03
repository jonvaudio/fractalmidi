module Main where

import Fractal
import NoteUtil
import Note

-- Simple example with manual rules

manualTestIntRules :: [Rule Int Int]
manualTestIntRules = [Rule 0 [(1, [0, 2, 4])],
                      Rule 1 [(1, [1, 3, 2])],
                      Rule 2 [(1, [4, 5, 6])],
                      Rule 3 [(1, [5, 4, 3])],
                      Rule 4 [(1, [0, 2, 1])],
                      Rule 5 [(1, [6, 6, 6])],
                      Rule 6 [(1, [4, 3, 2])] ]

manualTestNoteRules :: [Rule Int Note]
manualTestNoteRules = [Rule 0 [(1, [Note (Pitch C 5) 80 (1%8), Move (1%8)])],
                       Rule 1 [(1, [Note (Pitch D 5) 80 (1%8), Move (1%8)])],
                       Rule 2 [(1, [Note (Pitch E 5) 80 (1%8), Move (1%8)])],
                       Rule 3 [(1, [Note (Pitch F 5) 80 (1%8), Move (1%8)])],
                       Rule 4 [(1, [Note (Pitch G 5) 80 (1%8), Move (1%8)])],
                       Rule 5 [(1, [Note (Pitch A 5) 80 (1%8), Move (1%8)])],
                       Rule 6 [(1, [Note (Pitch B 5) 80 (1%8), Move (1%8)])] ]
             
manualTest = do ints <- growN 4 manualTestIntRules [0]
                notes <- grow manualTestNoteRules ints
                writeMIDIFile "oldTest1.mid" [Track 0 notes] [] 140
                
-- Simple example with automatic rules but not much coherence

aTestIntProfile = [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6)]
aTestDurationProfile = [(1, 1%4), (1, 1%8), (1, 1%16)]
aTestMelodyPitches =
    [Pitch C 5, Pitch D 5, Pitch E 5, Pitch F 5, Pitch G 5, Pitch A 5, Pitch B 5]
aTestBassPitches = shiftPitchPitches (-24) aTestMelodyPitches

aTest x =
 do seed x
    structure <- doBuildRules 2 aTestIntProfile aTestDurationProfile 2 [(0, 1%4)]
    abstractMelody <- doBuildRules 2 aTestIntProfile aTestDurationProfile 2 structure
    abstractBass <- doBuildRules 2 aTestIntProfile aTestDurationProfile 2 structure
    writeMIDIFile "oldTest2.mid" [Track 0 (doBuildNotesSimple aTestMelodyPitches abstractMelody),
                                  Track 1 (doBuildNotesSimple aTestBassPitches abstractBass)]
     [(1, 33)] 120
     
-- C Major
clDurations = [(7, (1%4)), (4, (1%8)), (9, (1%16))]
clDurations' = [(7, (1%4)), (4, (1%8)), (0, (1%16))]
clProfiles = [(1,
              [(5, 0), (1, 1), (5, 2), (1, 3), (5, 4), (1, 5), (5, 6), (1, 7), (5, 8), (1, 9),
               (5, 10), (1, 11), (1, 12), (1, 13)]
              )]
              
cls = [Pitch C 5, Pitch D 5, Pitch E 5, Pitch F 5, Pitch G 5, Pitch A 6, Pitch B 6]
clScale = cls ++ shiftPitchPitches 12 cls
clScaleLow = shiftPitchPitches (-24) clScale

chordlessTest x =
 do seed x
    structure <- generateStart 4 7 2 clDurations 2
    structureProfiled <- doAddProfiles 14 clDurations clProfiles structure
           
    basslineAbstract <- doBuildRulesProfiled 2 14 14 clDurations clProfiles 1 structureProfiled
    bassline <- return $ doBuildNotes clScaleLow basslineAbstract
            
    melodyAbstract <- doBuildRulesProfiled 2 14 7 clDurations clProfiles 1 structureProfiled
    melody <- return $ doBuildNotes clScale melodyAbstract
            
    writeMIDIFile "chordless.mid" [Track 0 bassline, Track 1 melody] [(0, 33)] 120

rotatingTest x =
 do seed x
    structure <- generateStart 4 7 2 clDurations 2
    structureProfiled <- doAddProfiles 14 clDurations clProfiles structure
           
    basslineAbstract <- doBuildRulesProfiled 2 14 14 clDurations clProfiles 1 structureProfiled
    bassline <- return $ doBuildNotes clScaleLow basslineAbstract
            
    melodyAbstract <- doBuildRulesProfiled 2 14 7 clDurations clProfiles 1 structureProfiled
    melody <- return $ doBuildNotes clScale melodyAbstract
    
    chords <- doBuildRotatingChords clScale 2 14 clDurations' clProfiles structureProfiled
    
    writeMIDIFile "rotating1.mid" [Track 0 bassline, Track 1 melody, Track 2 chords]
                  [(0, 33)] 120
                
-- Chapter 6 additional examples

-- Example 1

s = [Pitch C 5, Pitch Cs 5, Pitch D 5, Pitch Ds 5, Pitch E 5, Pitch F 5, Pitch Fs 5, Pitch G 5,
     Pitch Gs 5, Pitch A 5, Pitch As 5, Pitch B 5]
scale = s ++ shiftPitchPitches 12 s
scaleHigh = shiftPitchPitches 12 scale
scaleLow = shiftPitchPitches (-24) scale

profiles = [(1,
               [(5, 0), (5, 3), (5, 7), (5, 10), (5, 14), (5, 17)]
               ),
            (1,
               [(5, 0), (5, 4), (5, 8), (5, 10), (5, 14), (5, 17)]
               ),
            (1,
               [(5, 0), (5, 4), (5, 9), (5, 14), (5, 16)]
               )
           ]

durations = [(0, (4%4)), (0, (2%4)), (7, (1%4)), (4, (1%8)), (9, (1%16))]
structDurations = [(2, (4%4)), (2, (2%4))]
chordDurations = [(0, (4%4)), (1, (2%4)), (7, (1%4)), (6, (1%8)), (1, (1%16))]

test x =
 do seed x
    structure <- generateStart 4 12 2 structDurations 2
    structureProfiled <- doAddProfiles 24 durations profiles structure
            
    basslineAbstract <- doBuildRulesProfiled 2 24 24 durations profiles 1 structureProfiled
    bassline <- return $ doBuildNotes scaleLow basslineAbstract
            
    melodyAbstract <- doBuildRulesProfiled 2 24 12 durations profiles 1 structureProfiled
    melody <- return $ doBuildNotes scale melodyAbstract
            
    chords <- doBuildRotatingChords scale 2 24 chordDurations profiles structureProfiled
            
    writeMIDIFile "test.mid" [Track 0 chords, Track 1 bassline, Track 2 melody] [(1, 33)] 120

-- Example 2

structDurations2 = [(1, (4%4)), (1, (2%4)), (1, (1%4)), (1, (1%8))]
bassDurations2 = [(0, (4%4)), (5, 2%4), (3, 1%4), (3, 1%8), (5, 1%16)]
melodyDurations2 = [(5, 4%4), (0, 2%4), (3, 1%4), (3, 1%8), (5, 1%16)]

profiles2 = [(1, [
                  (5, 0), (5, 4), (5, 7), (5, 11)])]

test2 x =
 do seed x
    structure <- generateStart 4 12 2 structDurations2 2
    structureProfiled <- doAddProfiles 24 structDurations2 profiles2 structure
    
    basslineAbstract <- doBuildRulesProfiled 2 24 24 bassDurations2 profiles2 1 structureProfiled
    bassline <- return $ doBuildNotes scaleLow basslineAbstract
    
    melodyAbstract <- doBuildRulesProfiled 2 24 12 melodyDurations2 profiles2 1 structureProfiled
    melody <- return $ doBuildNotes scale melodyAbstract
    
    chords <- doBuildSimpleChords scale 2 24 structDurations2 profiles2 structureProfiled
    
    writeMIDIFile "test2.mid" [Track 0 chords, Track 1 bassline, Track 2 melody] [(1, 33)] 80
    
-- Example 3

structDurations3 = [(1, (4%4)), (1, (2%4)), (1, (1%4))]
bassDurations3 = [(0, (4%4)), (5, 2%4), (3, 1%4), (3, 1%12), (5, 2%12)]
melodyDurations3 = [(5, 4%4), (0, 2%4), (3, 1%4), (3, 1%12), (5, 2%12)]

profiles3 = [(1, [
                  (5, 0), (3, 2), (5, 3), (3, 7), (5, 11), (5, 17)]),
             (1, [(5, 0), (4, 4), (5, 7), (5, 10), (5, 14)
                  ])]

test3 x =
 do seed x
    structure <- generateStart 4 12 2 structDurations3 2
    structureProfiled <- doAddProfiles 24 structDurations3 profiles3 structure
    
    basslineAbstract <- doBuildRulesProfiled 2 24 24 bassDurations3 profiles3 1 structureProfiled
    bassline <- return $ doBuildNotes scaleLow basslineAbstract
    
    melodyAbstract <- doBuildRulesProfiled 2 24 12 melodyDurations3 profiles3 1 structureProfiled
    melody <- return $ doBuildNotes scale melodyAbstract
    
    melodyAbstract' <- doBuildRulesProfiled 2 24 12 melodyDurations3 profiles3 1 structureProfiled
    melody' <- return $ doBuildNotes scale melodyAbstract'
    
    chords <- doBuildRotatingChords scaleHigh 2 24 bassDurations3 profiles3 structureProfiled
    
    writeMIDIFile "test3.mid" [Track 0 chords, Track 1 bassline, Track 2 melody, Track 3 melody']
                  [(1, 33)] 145

-- This is not fractal music, but is a programmed drum line for testing DelayTrack stuff. Try
-- commenting out the different delay profiles.

testMIDI = writeMIDIFile "testmidi.mid" [DelayTrack 0 delayprofile 9 hh, DelayTrack 0 delayprofile 9 sd, DelayTrack 0 delayprofile 9 bd{-, DelayTrack 0 [(2%4, 200)] 9 click-}] [] 120
           where hh = concat $ take 256 $ repeat $ [Note (Pitch Fs 3) 50 (1%16), Move (1%8), Note (Pitch Fs 3) 30 (1%16), Move (1%16), Note (Pitch Fs 3) 30 (1%16), Move (1%16)]
                 sd = concat $ take 64 $ repeat $ [Move (1%16), Note (Pitch E 3) 30 (1%16), Move (3%16), Note (Pitch E 3) 100 (1%16), Move (1%16), Note (Pitch E 3) 30 (1%16), Move (2%16), Note (Pitch E 3) 30 (1%16), Move (2%16), Note (Pitch E 3) 30 (1%16), Move (1%16), Note (Pitch E 3) 30 (1%16), Move (2%16), Note (Pitch E 3) 100 (1%16), Move (1%16), Note (Pitch E 3) 30 (1%16), Move (2%16), Note (Pitch E 3) 30 (1%16), Move (1%16)]
                 bd = concat $ take 64 $ repeat $ [Note (Pitch C 3) 90 (1%16), Move (7%16), Note (Pitch C 3) 90 (1%16), Move (4%16), Note (Pitch C 3) 90 (1%16), Move (1%16), Note (Pitch C 3) 90 (1%16), Move (1%8), Note (Pitch C 3) 90 (1%16), Move (1%8)] 
                 click = concat $ take 128 $ repeat $ [Note (Pitch Cs 3) 100 (1%16), Move (1%4)]
                 --delayprofile = [(2%4, 25), (1%8, 25)]
                 --delayprofile = [(1%8, 50)]
                 delayprofile = []
