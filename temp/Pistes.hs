module Pistes (sumIPiste, sumPiste, concatPatrons, hashedPistes) where

import Instruments
import Structs
import Data.Map (Map, fromList)
import Utils
import DataSet
import Debug.Trace (trace)
  
concatPatrons :: [[Int]] -> [Double]  
concatPatrons [] = []
concatPatrons (x : xs) =(computePatron x ++ concatPatrons xs)
  
-- example computePatron [1, 2, 3]
computePatron :: [Int] -> [Double]
computePatron arr = let matrix = (getPisteMatrix arr) in sumPiste (length (head matrix) - 1) matrix

getPisteMatrix :: [Int] -> [[Double]]
getPisteMatrix [] = []
getPisteMatrix (x : xs) = trace ("getPisteMatrix") (let piste = getLogN x hashedPistes in [computePiste piste] ++ getPisteMatrix xs)

-- sum the column i
sumIPiste :: Int -> [[Double]] -> Double
sumIPiste _ [] = 0
sumIPiste i (x : xs)  = (x !! i) + sumIPiste i xs

sumPiste :: Int -> [[Double]] -> [Double]
sumPiste 0 arr = [sumIPiste 0 arr]
sumPiste i arr = sumPiste (i - 1) arr ++ [sumIPiste i arr]

computePiste :: Piste -> [Double]
computePiste piste = let instru = instructions piste in _computeInstruction instru
  
-- TODO: Calling createPeriode with the count argument as duration x considere that Dc (the first value in order is 1) 
_computeInstruction :: [Instruction] -> [Double] 
_computeInstruction [] = [] 
_computeInstruction (x : xs) = createPeriode x (floor (0.25*44100*(fromIntegral(duration x)))) ++ _computeInstruction xs

extractPisteId :: Piste -> (Int, Piste)
extractPisteId piste = (pisteId piste,piste)

-- Having to do O(n) each time we want to access a piste is too heavy
-- we can easily reduce to O(log(n)) using a Map
hashedPistes :: Map Int Piste
hashedPistes = fromList (map extractPisteId parsedPistes)


parsedPistes :: [Piste]
parsedPistes = parsePistes pistes

-- Parse a single piste
parsePiste :: String -> Int -> Int -> [String] -> [Instruction]
parsePiste prev instruId count arr | null arr          = trace ("parsePiste intruId: " ++ show instruId) [Instruction count instruId (parseNote (words prev))]
                                   | head arr == "-"   = parsePiste prev instruId (count + 1) (tail arr)
                                   | otherwise         = trace ("parsePiste intruId: " ++ show instruId) (Instruction count instruId (parseNote (words prev)) :
                                                            let n_piste = words (head arr) in
                                                              _halfParse instruId n_piste arr)
-- _halfParse ["A", "4", "1.0"] ["A 4 1.0"]
_halfParse :: Int -> [String] -> [String] -> [Instruction]
_halfParse instruId (x : xs) raw_lines | x == "silence" = parsePiste "silence" (-1) 1 (tail raw_lines)
                              | otherwise = parsePiste (head raw_lines) instruId 1 (tail raw_lines)


-- Group 0 ["A 4 1.0"] "piste 0"
--
_parsePiste :: Group -> Piste
_parsePiste group = trace ("_parsePiste" ++ show group) (Piste (index group) (
  let raw_lines = members group 
      instruId = (read (words (title group) !! 1)  :: Int) in 
    let n_piste = words (head raw_lines) in _halfParse instruId n_piste raw_lines) )

-- Parse a pistes files (argument is the lines)
parsePistes :: [String] -> [Piste]
parsePistes raw_lines = let groups = decompose "piste" raw_lines 0 in map _parsePiste groups