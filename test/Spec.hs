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
    null (getChildren "instrument" ["instrument"]),

    -- Testing members function of Group Data
    members (head $ decompose "instrument" instrumentsSample 0) == abc,
    members (decompose "instrument" instrumentsSample 0 !! 1) == ["D"],

    -- Testing Index function of Group Data
    index (head $ decompose "instrument" instrumentsSample 0) == 0,
    index (decompose "instrument" instrumentsSample 0 !! 1) == 1,

    -- Testing parsePiste function
    length (parsePiste "silence" 1 [
        "-"
      , "-"
      , "A 1 3.0"
    ]) == 2,
    
    duration (head (parsePiste "silence" 1 [
            "-"
          , "-"
          , "A 1 3.0"
        ])) == 3,
        
    duration (parsePiste "silence" 1 [
                "-"
              , "-"
              , "A 1 3.0"
            ] !! 1) == 1,
    
    content (head (parsePiste "silence" 1 [
                "-"
              , "-"
              , "A 1 3.0"
            ])) == "silence",
            
    content (parsePiste "silence" 1 [
                    "-"
                  , "-"
                  , "A 1 3.0"
                ] !! 1) == "A 1 3.0",
                
    length (parsePistes pistes) == 3,
    let instru = head (instructions (head (parsePistes pistes))) in duration instru == 3 && content instru == "silence"
  ]

-- This function will ensure all the assert in tests are true, otherwise will raise an error
runTests :: [Bool] -> Int -> Bool
runTests [] _ = True
runTests (x : xs) i | x = runTests xs (i + 1)
                    | otherwise = error ("The test at index " ++ show i ++ " failed")


-- == Function related to the TP1 ==


-- Here is the value than must be at the first line of our order.txt
durationInstruction :: Double
durationInstruction = 2.0

patrons :: [String]
patrons = [
    "0 1 2"
  , "2 1"
  ]

pistes :: [String]
pistes = [
    "piste" -- index 0
  , "silence"
  , "-"
  , "-"
  , "A 1 3.0"
  , "-"
  , "piste" -- index 1
  , "silence"
  , "piste" -- index 2
  , "silence"
  ]

newtype Piste = Piste {instructions :: [Instruction]}

data Instruction = Instruction
  {
      duration :: Int
  ,   content :: String
  }


parsePiste :: String -> Int -> [String] -> [Instruction]
parsePiste prev count arr | null arr          = [Instruction count prev]
                          | head arr == "-"   = parsePiste prev (count + 1) (tail arr)
                          | otherwise         = Instruction count prev : parsePiste (head arr) 1 (tail arr)

_parsePiste :: Group -> Piste
_parsePiste group = Piste (let raw_lines = members group in parsePiste (head raw_lines) 1 (tail raw_lines))

parsePistes :: [String] -> [Piste]
parsePistes raw_lines = let groups = decompose "piste" raw_lines 0 in map _parsePiste groups




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





-- échantillon
-- ondes  : liste de fonction d'ondes non parsé ex; ["sin 3 8 5 5", "pulse 4 5 5 5"...]
-- volume : valeur du volume
-- temps  : valeur du temps
echantillon :: [String] -> Double -> Double -> Double
echantillon ondes volume temp = volume * 1

computerOnde :: String -> Double -> Double -> Double
computerOnde onde f t = parseFunction (words onde) f t


-- Take an array and an index as argument
-- return the value at index given parsed as a double
asDouble :: [String] -> Int -> Double
asDouble arr n = read (arr !! n):: Double


-- [String] line : ex ["sin","3"...]
-- Frequence
-- time
parseFunction :: [String] -> Double -> Double -> Double
parseFunction (x : xs) f t | x == "sin"                        = functionSin   (FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) 0 (asDouble xs 3)) f t
                           | x == "pulse"                      = functionPulse ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4)) f t
                           | x == "triangle"                   = functionTriangle ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4)) f t
                           | otherwise                         = error "The function is not recognized."

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
demiTons _     = error "The note was not recognised."

-- This are the param used for every function

data FParam = FParam
  {
    e :: Double
  , a1 :: Double
  , a2 :: Double
  , q :: Double -- this will be equal to 0 when used for the sinus
  , phi :: Double
  }

functionSin :: FParam -> Double -> Double -> Double
functionSin param f t = ebl param t * sin (2 * pi * f * t + phi param)
  
functionPulse :: FParam -> Double -> Double -> Double
functionPulse param f t | delta param t f < q param = ebl param t
                        | otherwise                 = negate (ebl param t)
  
functionTriangle :: FParam -> Double -> Double -> Double
functionTriangle param f t | delta param t f < q param = ebl param t * ((2 * delta param t f) / q param - 1)
                           | otherwise                 = ebl param t * (1 - ((2*(delta param t f - q param))/(1 - q param)))
  
ebl :: FParam -> Double -> Double
ebl param t | t <= e param = a1 param
            | otherwise    = a2 param
            
deltaX :: FParam -> Double -> Double -> Double
deltaX param t f = (t + phi param) * f

delta :: FParam -> Double -> Double -> Double
delta param t f = let x = deltaX param t f in x - fromInteger (floor x)