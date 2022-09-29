main :: IO ()
main = do
   -- orders <- readFile "./test/instruments.txt"
   -- First extracts the lines of order
   -- let raw_lines = lines orders
   -- print (read (head raw_lines) :: Double)
   --
   -- members ((decompose "piste" ["piste", "silence", "-", "A 2 3", "-"] 0) !! 0) == ["silence","-","A 2 3","-"]
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
    length (parsePiste "silence" 1 1 [
        "-"
      , "-"
      , "A 1 3.0"
    ]) == 2,

    duration (head (parsePiste "silence" 1 1 [
            "-"
          , "-"
          , "A 1 3.0"
        ])) == 3,

    duration (parsePiste "silence" 1 1 [
                "-"
              , "-"
              , "A 1 3.0"
            ] !! 1) == 1,

    length parsedPistes == 3,
    length (instructions (head parsedPistes)) == 2,
    duration (head $ instructions (head parsedPistes)) == 3,
    duration (instructions (head parsedPistes) !! 1) == 2,
    volume (note (head $ instructions (head parsedPistes))) == 0,
    volume (note (instructions (head parsedPistes) !! 1)) == 3.0,

    -- Testing parseInstrument
    length (parseInstrument instruments) == 3,
    instrumentId (head $ parseInstrument instruments) == 0,
    instrumentId (parseInstrument instruments !! 1) == 1,
    length (ondes (parseInstrument instruments !! 1)) == 2,
    -- Compare 
    head (ondes (head (parseInstrument instruments))) 0.0 0.0 == functionSin (FParam 1.0 2.0 3.0 0.0 4.0)  0 0,
    (ondes (head (parseInstrument instruments)) !! 1) 0.0 0.0 == functionPulse (FParam 1.0 2.0 3.0 4.0 5.0)  0 0,
    (head (ondes ((parseInstrument instruments) !! 2))) 0.0 0.0 == functionTriangle (FParam 1.0 2.0 3.0 4.0 5.0)  0 0
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
      duration :: Int -- number of "-" + 1
  ,   instrument :: Int -- instrument id (index)
  ,   note :: Note -- the note
  }

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
_parsePiste group = Piste (let raw_lines = members group in let n_piste = words (head raw_lines) in
                                                              parsePiste (head raw_lines) (read (n_piste !! 1) :: Int) 1 (tail raw_lines))

-- Parse a pistes files (argument is the lines)
parsePistes :: [String] -> [Piste]
parsePistes raw_lines = let groups = decompose "piste" raw_lines 0 in map _parsePiste groups

instruments :: [String]
instruments = [
      "instrument"
    , "sin 1.0 2.0 3.0 4.0"
    , "pulse 1.0 2.0 3.0 4.0 5.0"
    , "instrument"
    , "pulse 1.0 2.0 3.0 4.0 5.0"
    , "sin 1.0 2.0 3.0 4.0"
    , "instrument"
    , "triangle 1.0 2.0 3.0 4.0 5.0"
    , "sin 1.0 2.0 3.0 4.0"
  ]

data Instrument = Instrument
    {
      instrumentId :: Int
    , ondes :: [Double -> Double -> Double]
    }


groupToInstrument :: Group -> Instrument
groupToInstrument group = Instrument (index group) (map (parseFunction.words) (members group))

parseInstrument :: [String] -> [Instrument]
parseInstrument arr = let groups = decompose "instrument" arr 0 in map groupToInstrument groups






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
parseFunction :: [String] -> Double -> Double -> Double
parseFunction (x : xs) | x == "sin"                        = functionSin   (FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) 0 (asDouble xs 3))
                       | x == "pulse"                      = functionPulse ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4))
                       | x == "triangle"                   = functionTriangle ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4))
                       | otherwise                         = error "The function is not recognized."


data Note = Note
  {
      frequence :: Double
  ,   volume    :: Double
  }

parseNote :: [String] -> Note
parseNote (x : xs) | x == "silence" = Note 0 0
                   | otherwise      = Note (computeFrequence (computehauteur (read (head xs) :: Double) x)) (read (xs !! 1) :: Double)

-- Calcul la fréquence 
-- f = 440*z^hauteur avec z = 2^(1/12)
computeFrequence :: Double -> Double
computeFrequence h = 440.0 * (2 ** (1 /12)) ** h

-- Calcul de la hauteur 
-- h = 12*c + u - 57
computehauteur :: Double -> String -> Double
computehauteur octave note = 12.0 * octave + demiTons note - 57

-- Demi ton (from table)
demiTons :: String -> Double
demiTons "C"   = 0.0
demiTons "C#"  = 1.0
demiTons "D"   = 2.0
demiTons "D#"  = 3.0
demiTons "E"   = 4.0
demiTons "F"   = 5.0
demiTons "F#"  = 6.0
demiTons "G"   = 7.0
demiTons "G#"  = 8.0
demiTons "A"   = 9.0
demiTons "A#"  = 10.0
demiTons "B"   = 11.0
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