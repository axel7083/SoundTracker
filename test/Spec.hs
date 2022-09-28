main :: IO ()
main = do
   -- orders <- readFile "./test/instruments.txt"
   -- First extracts the lines of order
   -- let raw_lines = lines orders
   -- print (read (head raw_lines) :: Double)
   print $ runTests tests 0
   

abc :: [String]
abc = ["A", "B", "C"]

instrumentsSample :: [String]
instrumentsSample = ["instrument"] ++ abc ++ ["instrument", "D"]
   
tests :: [Bool]
tests = [
    -- Test relative to the getChildren function
    getChildren "instrument" instrumentsSample ==  abc,
    null (getChildren "instrument" ["instrument"]),
    members (head $ decompose "instrument" instrumentsSample 0) == abc,
    members (decompose "instrument" instrumentsSample 0 !! 1) == ["D"]
  ]

-- This function will ensure all the assert in tests are true, otherwise will raise an error
runTests :: [Bool] -> Int -> Bool
runTests [] _ = True
runTests (x : xs) i | x = runTests xs (i + 1)
                    | otherwise = error ("The test at index " ++ show i ++ " failed")


-- This method will take two argument, a stop condition and an array
-- Example getChildren ( "S" ["A", "B", "S", "C"] ) = ["A", "B"]
getChildren :: String -> [String] -> [String]
getChildren _ [] = []
getChildren stop (x : xs) | stop == x = []
                          | otherwise = x : getChildren stop xs

data Group = Group 
  {
    index :: Int
  , members :: [String]
  }


decompose :: String -> [String] -> Int -> [Group]
decompose _ [] _ = []
decompose condition (x : xs) i  | x == condition = Group 1 (getChildren condition xs) : decompose condition xs (i + 1)
                                    | otherwise = decompose condition xs i





