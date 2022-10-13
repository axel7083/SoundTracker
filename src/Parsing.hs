module Parsing (parseInstruments, parseComposition, parsePistes) where

import Structures

-- ==================================================================
-- Parsing instrument file
-- ==================================================================  

parseInstruments :: String -> [Instrument]
parseInstruments content = instruments
                            where
                              groups = decompose (\x -> x == "instrument") (lines content) 0
                              instruments = [
                                Instrument 
                                  (index group)                                -- instrument id
                                  (map (parseFunction.words) (members group))  -- functions
                                  | group <- groups                            -- iterator
                                ]

asDouble :: [String] -> Int -> Double
asDouble arr n = read (arr !! n):: Double

parseFunction :: [String] -> Double -> Double -> Double
parseFunction (x : xs) | x == "sin"                        = functionSin   (FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) 0 (asDouble xs 3))
                       | x == "pulse"                      = functionPulse ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4))
                       | x == "triangle"                   = functionTriangle ( FParam (asDouble xs 0) (asDouble xs 1) (asDouble xs 2) (asDouble xs 3) (asDouble xs 4))
                       | otherwise                         = error "The function is not recognized."

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

-- ==================================================================
-- Parsing composition file
-- ==================================================================  

-- | Parse a composition file
-- The first line is a Double value, and each following line is a list of Integer separated by a space
parseComposition :: String -> (Double, [[Int]])
parseComposition content = (speed, patrons)
                            where
                              raw_lines = lines content
                              speed = read (head raw_lines) :: Double
                              patrons = [map (\y -> read y :: Int) (words x) | x <- tail raw_lines]

-- ==================================================================
-- Parsing pistes
-- ==================================================================  
  
parsePistes :: String -> [Piste]
parsePistes content = pistes
                        where
                          groups = decompose (\x -> head (words x) == "piste") (lines content) 0
                          pistes = [
                            Piste
                              (index group)                               -- Piste Id
                              (read (words (title group) !! 1) :: Int)    -- Instrument Id
                              (parseInstructions (members group))         -- Instructions
                              | group <- groups                           -- Iterator
                            ]

-- | Parsing instructions form raw lines
-- ==Example==
-- silence
-- -
-- A 1 3.0
-- -
-- -
-- B 1 3.0
parseInstructions :: [String] -> [Instruction]
parseInstructions raw_lines = instrus
                                where
                                  groups = decompose (\x -> x /= "-") raw_lines 0
                                  instrus = [
                                    Instruction
                                      (length m + 1)
                                      (parseNote (words t))
                                      | Group {members = m, title = t} <- groups
                                    ]

parseNote :: [String] -> Note
parseNote (x : xs) | x == "silence" = Note 0 0
                     | otherwise      = Note 
                                        (computeFrequency (computeHeight (read (head xs) :: Double) x)) 
                                        (read (xs !! 1) :: Double)

-- Calcul la fréquence
-- f = 440*z^hauteur avec z = 2^(1/12)
computeFrequency :: Double -> Double
computeFrequency h = 440.0 * (2 ** (1 /12)) ** h

-- Calcul de la hauteur
-- h = 12*c + u - 57
computeHeight :: Double -> String -> Double
computeHeight octave _note = 12.0 * octave + demiTons _note - 57

-- | Demi ton (from table)
demiTons :: String -> Double
demiTons i = case i of
  "C"   -> 0.0
  "C#"  -> 1.0
  "D"   -> 2.0
  "D#"  -> 3.0
  "E"   -> 4.0
  "F"   -> 5.0
  "F#"  -> 6.0
  "G"   -> 7.0
  "G#"  -> 8.0
  "A"   -> 9.0
  "A#"  -> 10.0
  "B"   -> 11.0
  _     -> error "The note was not recognised."

-- ==================================================================
-- Utility functions
-- ==================================================================  

-- | This will return a list of Group were each group contain the lines between the stop condition
-- Example
-- (decompose "A" ["A", "1", "2", "A", "3"] 0) will return [Group 0 ["1", "2"], Group 1 ["3"]]
decompose :: (String -> Bool) -> [String] -> Int -> [Group]
decompose _ [] _ = []
decompose f (x : xs) i  | f x       = Group i (takeWhile f xs) x : decompose f xs (i + 1)
                        | otherwise = decompose f xs i