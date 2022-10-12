module Main (main) where

fibn :: Int -> Int
fibn n = fibs !! n
           where
           fibs = 0 : 1 : map f [2..]
           f a = fibs !! (a-1) + fibs !! (a-2)

main :: IO ()
main =
    
    print (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in take 10 fibs)
