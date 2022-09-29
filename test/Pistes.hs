module Pistes where

import Data.Map (Map, fromList)
import Instruments
import Utils
import DataSet

data Instruction = Instruction
  {
      duration :: Int -- number of "-" + 1
  ,   instrument :: Int -- instrument id (index)
  ,   note :: Note -- the note
  }

data Piste = Piste {
    pisteId :: Int  
  , instructions :: [Instruction]
}
    
-- example computePatron [1, 2, 3]
computePatron :: [Int] -> [Double]
computePatron [] = []
computePatron (x : xs) = let piste = getLogN x hashedPistes in [] ++ computePatron xs

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
parsePiste prev instruId count arr | null arr          = [Instruction count instruId (parseNote (words prev))]
                                   | head arr == "-"   = parsePiste prev instruId (count + 1) (tail arr)
                                   | otherwise         = Instruction count instruId (parseNote (words prev)) :
                                                            let n_piste = words (head arr) in
                                                              parsePiste (head arr) (read (n_piste !! 1) :: Int) 1 (tail arr)

_parsePiste :: Group -> Piste
_parsePiste group = Piste (index group) (let raw_lines = members group in let n_piste = words (head raw_lines) in
                                                              parsePiste (head raw_lines) (read (n_piste !! 1) :: Int) 1 (tail raw_lines))

-- Parse a pistes files (argument is the lines)
parsePistes :: [String] -> [Piste]
parsePistes raw_lines = let groups = decompose "piste" raw_lines 0 in map _parsePiste groups