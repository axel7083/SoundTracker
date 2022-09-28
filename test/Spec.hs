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

    -- Testing members function of Group Data
    members (head $ decompose "instrument" instrumentsSample 0) == abc,
    members (decompose "instrument" instrumentsSample 0 !! 1) == ["D"],

    -- Testing Index function of Group Data
    index (head $ decompose "instrument" instrumentsSample 0) == 0,
    index (decompose "instrument" instrumentsSample 0 !! 1) == 1
  ]

-- This function will ensure all the assert in tests are true, otherwise will raise an error
runTests :: [Bool] -> Int -> Bool
runTests [] _ = True
runTests (x : xs) i | x = runTests xs (i + 1)
                    | otherwise = error ("The test at index " ++ show i ++ " failed")


-- == Function related to the TP1 ==

-- This method will take two argument, a stop condition and an array
-- Example getChildren ( "S" ["A", "B", "S", "C"] ) = ["A", "B"]
getChildren :: String -> [String] -> [String]
getChildren _ [] = []
getChildren stop (x : xs) | stop == x = []
                          | otherwise = x : getChildren stop xs

-- When parsing a file we need to group the content
data Group = Group
  {
    index :: Int
  , members :: [String]
  }

-- This will return a list of Group were each group contain the lines between the stop condition
-- Example
-- (decompose "A" ["A", "1", "2", "A", "3"] 0) will return [Group 0 ["1", "2"], Group 1 ["3"]]
decompose :: String -> [String] -> Int -> [Group]
decompose _ [] _ = []
decompose condition (x : xs) i  | x == condition  = Group i (getChildren condition xs) : decompose condition xs (i + 1)
                                | otherwise       = decompose condition xs i


parseFunction :: [String] -> Double
parseFunction (x : xs) | x == "sin"      = sin 3
                       | x == "pulse"    = 2
                       | x == "triangle" = 8
                       | otherwise       = error "The function is not recognized."


-- Calcul la fréquence 
-- f = 440*z^hauteur avec z = 2^(1/12)
frequence :: Double -> Double
frequence hauteur = 440.0 * (2 ** (1 /12)) ** hauteur

-- Calcul de la hauteur 
-- h = 12*c + u - 57
hauteur :: Int -> String -> Int
hauteur octave note = 12 * octave + (demiTons note) - 57

-- Demi ton (from table)
demiTons :: String -> Int
demiTons "C"   = 0
demiTons "C#"  = 1
demiTons "D"   = 2
demiTons "D#"  = 3
demiTons "E"   = 4
demiTons "F"   = 5
demiTons "F#"  = 6
demiTons "G"   = 7
demiTons "G#"  = 8
demiTons "A"   = 9
demiTons "A#"  = 10
demiTons "B"   = 11
demiTons _ = error "The note was not recognised."

