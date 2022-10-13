module Main (main) where
import Data.List (transpose)

main :: IO ()
main = do
    print (concat patrons)
      where 
        patrons = [ computePatron x | x <- [[2, 2], [1]] ] 

-- Compute the patron 
computePatron :: [Int] -> [Double]
computePatron pistes = sumColumns (map computePiste pistes)

-- Partial application
-- Sum the column of a 2 dimensions matrix
-- sumColumns [[1, 2, 3], [4, 5, 6]] = [5, 7, 9]
sumColumns :: [[Double]] -> [Double]
sumColumns = map sum . transpose

computePiste :: Int -> [Double]
computePiste i = [fromIntegral i]