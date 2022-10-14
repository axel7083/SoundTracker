module Utils (getLogN, decompose) where

import Structs
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Debug.Trace (trace)

-- Here is our help function to access any map
getLogN :: Int -> Map Int a -> a
getLogN i m =fromJust $ lookup i m

-- This method will take two argument, a stop condition and an array
-- Example getChildren ( "S" ["A", "B", "S", "C"] ) = ["A", "B"]
getChildren :: String -> [String] -> [String]
getChildren _ [] = []
getChildren stop (x : xs) | stop == head (words x) = []
                          | otherwise = x : getChildren stop xs


-- This will return a list of Group were each group contain the lines between the stop condition
-- Example
-- (decompose "A" ["A", "1", "2", "A", "3"] 0) will return [Group 0 ["1", "2"], Group 1 ["3"]]
decompose :: String -> [String] -> Int -> [Group]
decompose _ [] _ = []
decompose condition (x : xs) i  | head (words x) == condition = Group i (getChildren condition xs) x: decompose condition xs (i + 1)
                                | otherwise       = decompose condition xs i