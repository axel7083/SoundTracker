import Pistes
import Instruments
import Utils
import Structs

import Data.Int
import Data.ByteString.Builder
import qualified GHC.IO.Handle
import qualified GHC.IO.Handle.FD
import qualified GHC.IO.Handle.Types
import qualified GHC.IO.IOMode
import System.Environment
import Lib

------------------------------------------------------------------------
-- Le code suivant construit le fichier .WAV.                         --
-- Le code que vous devez construire est à la suite.                  --
-- Remarquez les constantes                                           --
--     _FREQUENCE_ECHANTILLONAGE                                      --
--     _AMPLITUDE_MAXIMUM                                             --
-- qui vous seront utile.                                             --
------------------------------------------------------------------------

_FREQUENCE_ECHANTILLONAGE :: Int32
_FREQUENCE_ECHANTILLONAGE = 44100

_FREQUENCE_ECHANTILLONAGE_DOUBLE :: Double
_FREQUENCE_ECHANTILLONAGE_DOUBLE = fromIntegral _FREQUENCE_ECHANTILLONAGE

_FREQUENCE_ECHANTILLONAGE_INT :: Int
_FREQUENCE_ECHANTILLONAGE_INT = fromIntegral _FREQUENCE_ECHANTILLONAGE

_AMPLITUDE_MAXIMUM :: Int
_AMPLITUDE_MAXIMUM = ( 2 ^ ( ( ( fromIntegral _OCTETS_PAR_ECHANTILLON ) :: Int ) * 8 - 1) ) - 1

_AMPLITUDE_MAXIMUM_DOUBLE :: Double
_AMPLITUDE_MAXIMUM_DOUBLE = fromIntegral _AMPLITUDE_MAXIMUM

------------------------------------------------------------------------

_POSITION_ARG_NOM_FICHIER_INSTRUMENTS :: Int
_POSITION_ARG_NOM_FICHIER_INSTRUMENTS = 0

_POSITION_ARG_NOM_FICHIER_TRACKS :: Int
_POSITION_ARG_NOM_FICHIER_TRACKS = 1

_POSITION_ARG_NOM_FICHIER_COMPOSITION :: Int
_POSITION_ARG_NOM_FICHIER_COMPOSITION = 2

_POSITION_ARG_NOM_FICHIER_WAVE :: Int
_POSITION_ARG_NOM_FICHIER_WAVE = 3

_MSSG_ERREUR_NOMBRE_ARGUMENT :: String
_MSSG_ERREUR_NOMBRE_ARGUMENT = "Il doit y avoir quatre arguments sur la ligne de commande."


_OCTETS_PAR_ECHANTILLON :: Int16
_OCTETS_PAR_ECHANTILLON = 2


-- -- entete info
-- RIFF
tailleDonnee :: Int -> Int32
tailleDonnee nbEchantillon = 20 + _SUB_1_TAILLE + ( _SUB_2_TAILLE nbEchantillon )

-- WAVE
chunkBuiler :: [ Int ] -> Builder
chunkBuiler echantillons = mconcat [ string8 "RIFF",
                                     int32LE ( tailleDonnee nbEchantillon ),
                                     string8 "WAVE",
                                     _SUB_1_BUILDER,
                                     sub2Builder nbEchantillon echantillons
                                   ]
    where nbEchantillon = length echantillons

-- -- Sub 1 info
-- fmt
_SUB_1_TAILLE :: Int32
_SUB_1_TAILLE = 16

-- PCM
_FORMAT_AUDIO :: Int16
_FORMAT_AUDIO = 1

-- Mono
_NB_CANALS :: Int16
_NB_CANALS = 1

_FREQUENCE_OCTETS :: Int32
_FREQUENCE_OCTETS = _FREQUENCE_ECHANTILLONAGE *
                  ( ( fromIntegral _NB_CANALS ) :: Int32 ) *
                  ( ( fromIntegral _OCTETS_PAR_ECHANTILLON ) :: Int32 )

_OCTETS_PAR_BLOCK :: Int16
_OCTETS_PAR_BLOCK = _NB_CANALS * _OCTETS_PAR_ECHANTILLON

_BITS_PAR_ECHANTILLON :: Int16
_BITS_PAR_ECHANTILLON = _OCTETS_PAR_ECHANTILLON * 8

_SUB_1_BUILDER :: Builder
_SUB_1_BUILDER = mconcat [ string8 "fmt ",
                        int32LE _SUB_1_TAILLE,
                        int16LE _FORMAT_AUDIO,
                        int16LE _NB_CANALS,
                        int32LE _FREQUENCE_ECHANTILLONAGE,
                        int32LE _FREQUENCE_OCTETS,
                        int16LE _OCTETS_PAR_BLOCK,
                        int16LE _BITS_PAR_ECHANTILLON
                      ]

-- -- Sub 2 info
-- data
_SUB_2_TAILLE :: Int -> Int32
_SUB_2_TAILLE nbEchantillon = ( ( fromIntegral nbEchantillon ) :: Int32 ) *
                           ( ( fromIntegral _OCTETS_PAR_BLOCK ) :: Int32 )

sub2Builder :: Int -> [ Int ] -> Builder
sub2Builder nbEchantillon echantillons = mconcat ( [ string8 "data",
                                                     int32LE ( _SUB_2_TAILLE nbEchantillon )
                                                   ] ++ [ fTraduction x | x <- echantillons ] )
    where fTraduction = if _OCTETS_PAR_ECHANTILLON == 1
                        then \x -> int8 ( ( fromIntegral x ) :: Int8 )
                        else \x -> int16LE ( ( fromIntegral x ) :: Int16 )

ecrireWave :: FilePath -> [ Int ] -> IO ()
ecrireWave nomFichier echantillons =
    do fichier <- GHC.IO.Handle.FD.openBinaryFile nomFichier GHC.IO.IOMode.WriteMode
       GHC.IO.Handle.hSetBuffering fichier ( GHC.IO.Handle.Types.BlockBuffering Nothing )
       hPutBuilder fichier ( chunkBuiler echantillons )
       GHC.IO.Handle.hClose fichier

born :: Int -> Int
born value | value > 32767 = 32767
           | value < (-32767) = (-32767)
           | otherwise = value

tronc :: Double -> Int
tronc value = (born.floor) (value * 32767)

main :: IO ()
main = do
   putStr "wait lol"
   ecrireWave "./frerejacqueTest.wav" (map tronc (concatPatrons [[0]
    ,[0]
    ,[1]
    ,[1]
    ,[2]
    ,[2]
    ,[3]
    ,[3] ]))

tests :: [Bool]
tests = [
    -- sumIPiste 0 ([ [1,2,3], [4,5,6] ]) == 5.0,
    -- sumIPiste 1 ([ [1,2,3], [4,5,6] ]) == 7.0,
    -- sumIPiste 2 ([ [1,2,3], [4,5,6] ]) == 9.0,
    -- sumPiste 2 ([ [1,2,3], [4,5,6] ]) == [5.0, 7.0, 9.0]

    -- length (ondes (getLogN 0 hashedInstruments)) == 2
    -- length (instructions (getLogN 1 hashedPistes)) == 3

    -- Test relative to the getChildren function
    -- null (getChildren "instrument" ["instrument"]),

    -- Testing members function of Group Data
    -- members (head $ decompose "instrument" instrumentsSample 0) == abc,
    -- members (decompose "instrument" instrumentsSample 0 !! 1) == ["D"],

    -- Testing Index function of Group Data
    -- index (head $ decompose "instrument" instrumentsSample 0) == 0,
    -- --index (decompose "instrument" instrumentsSample 0 !! 1) == 1,


    -- Testing parsePiste function
    --length (parsePiste "silence" 1 1 [
    --    "-"
    --  , "-"
    --  , "A 1 3.0"
    --]) == 2,
--
    --duration (head (parsePiste "silence" 1 1 [
    --        "-"
    --      , "-"
    --      , "A 1 3.0"
    --    ])) == 3,
--
    --duration (parsePiste "silence" 1 1 [
    --            "-"
    --          , "-"
    --          , "A 1 3.0"
    --        ] !! 1) == 1,
--
    --length parsedPistes == 3,
    --length (instructions (head parsedPistes)) == 2,
    --duration (head $ instructions (head parsedPistes)) == 3,
    --duration (instructions (head parsedPistes) !! 1) == 2,
    --volume (note (head $ instructions (head parsedPistes))) == 0,
    --volume (note (instructions (head parsedPistes) !! 1)) == 3.0,
--
    ---- Testing parseInstrument
    --length parsedInstruments == 3,
    --instrumentId (head parsedInstruments) == 0,
    --instrumentId (parsedInstruments !! 1) == 1,
    --length (ondes (parsedInstruments !! 1)) == 2,
--
    ---- Compare function sin pulse and triangle
    --head (ondes (head parsedInstruments)) 0.0 0.0 == functionSin (FParam 1.0 2.0 3.0 0.0 4.0)  0 0,
    --(ondes (head parsedInstruments) !! 1) 0.0 0.0 == functionPulse (FParam 1.0 2.0 3.0 4.0 5.0)  0 0,
    --head (ondes (parsedInstruments !! 2)) 0.0 0.0 == functionTriangle (FParam 1.0 2.0 3.0 4.0 5.0)  0 0,
    --
    ---- Testing map function
    --pisteId (getLogN 0 hashedPistes) == 0,
    --pisteId (getLogN 1 hashedPistes) == 1,
    --pisteId (getLogN 2 hashedPistes) == 2
  ]

-- This function will ensure all the assert in tests are true, otherwise will raise an error
runTests :: [Bool] -> Int -> Bool
runTests [] _ = True
runTests (x : xs) i | x = runTests xs (i + 1)
                    | otherwise = error ("The test at index " ++ show i ++ " failed")