module Instruments (parseNote, computeNote) where

import Utils
import DataSet

import Structs



-- currently need to be reverse since we start by the end (using recursive)
createPeriode :: Instruction -> Instrument -> Int -> [Double]
createPeriode _ _ 0 = []
createPeriode instruction instrument count = [computeNote instruction instrument (fromIntegral count)] ++ createPeriode instruction instrument (count - 1)

computeNote :: Instruction -> Instrument -> Double -> Double
computeNote instruction instrument time = let _note = note instruction in
                                        volume _note * sum (map (\x -> x (frequence _note) time) (ondes instrument))
                                                                                                          
parsedInstruments :: [Instrument]
parsedInstruments = parseInstrument instruments

groupToInstrument :: Group -> Instrument
groupToInstrument group = Instrument (index group) (map (parseFunction.words) (members group))

parseInstrument :: [String] -> [Instrument]
parseInstrument arr = let groups = decompose "instrument" arr 0 in map groupToInstrument groups

-- échantillon
-- ondes  : liste de fonction d'ondes non parsé ex; ["sin 3 8 5 5", "pulse 4 5 5 5"...]
-- volume : valeur du volume
-- temps  : valeur du temps
-- echantillon :: [String] -> Double -> Double -> Double
-- echantillon ondes volume temp = volume * 1

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
