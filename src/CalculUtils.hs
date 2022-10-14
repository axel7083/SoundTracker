module CalculUtils (computePatron) where

import Data.List
import FileUtils
import Structures
import Data.Maybe (fromJust)
import Data.Int
import Parsing

-- | Compute the patron
computePatron :: [Int] -> [Piste] -> [Instrument] -> Double -> [Int]
computePatron patron pistesRef instrumentsRef period = sumColumns (map (computePiste instrumentsRef period.getPiste pistesRef) patron)

-- | Given a list of pistes
-- it retrieve the piste with the pisteId corresponding to the value given as second argument
-- Will produce an error if the piste is not found.
getPiste :: [Piste] -> Int -> Piste
getPiste pistes i = fromJust (find (\x -> pisteId x == i) pistes)

-- | Given a list of instruments
-- it retrieve the instrument ...
getInstrument :: [Instrument] -> Int -> Instrument
getInstrument instruments i = fromJust (find (\x -> instrumentId x == i) instruments)

bound :: Double -> Int
bound value | value > 32767 = 32767
            | value < (-32767) = -32767
            | otherwise = floor value

-- | Sum the column of a 2 dimensions matrix
-- sumColumns [[1, 2, 3], [4, 5, 6]] = [5, 7, 9]
-- Partial application
sumColumns :: [[Double]] -> [Int]
sumColumns = map (bound.(*32767).sum) . transpose  

-- | Compute the corresponding values for a piste
computePiste :: [Instrument] -> Double -> Piste -> [Double]
computePiste instruments period (Piste _pisteId _instrumentId _instructions) = values
                                                           where
                                                             _instrument = getInstrument instruments _instrumentId
                                                             values = concatMap (computeInstruction period _instrument) _instructions

-- | Compute an instruction in a piste
-- All the instruction will be concat to create the piste
computeInstruction :: Double -> Instrument -> Instruction -> [Double]
computeInstruction period _instrument (Instruction _duration (Note _frequency _volume))
                  | _volume == 0.0 = replicate (computeDuration _duration period) 0.0
                  | otherwise = values
                                  where
                                    count = [0, 1 .. computeDuration _duration period]
                                    values = [
                                      computeNote
                                        _instrument
                                        ((fromIntegral t :: Double) / fromIntegral _FREQUENCE_ECHANTILLONAGE :: Double)
                                        _volume
                                        _frequency
                                      | t <- count
                                      ]

-- | This function allow us to compute the duration of an isntruction
computeDuration :: Int -> Double -> Int
computeDuration _duration period = floor (period*(fromIntegral _duration :: Double)*(fromIntegral _FREQUENCE_ECHANTILLONAGE :: Double))

-- | This function allow us to compute the value of a certain note at a given time
computeNote :: Instrument -> Double -> Double -> Double -> Double
computeNote Instrument {ondes=_ondes} t _volume _frequency = _volume * sum (map (\x -> x _frequency t) _ondes)