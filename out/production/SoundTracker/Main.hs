module Main (main) where

import System.Environment
import CalculUtils
import FileUtils
import Parsing

-------------------------------------------------------------------------------
main :: IO ()
main = 
    do argv <- getArgs
       argc <- return ( length argv )
       nomFichierInsts <- if _POSITION_ARG_NOM_FICHIER_INSTRUMENTS < argc 
                          then return ( argv !! _POSITION_ARG_NOM_FICHIER_INSTRUMENTS )
                          else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierTracks <- if _POSITION_ARG_NOM_FICHIER_TRACKS < argc 
                           then return ( argv !! _POSITION_ARG_NOM_FICHIER_TRACKS )
                           else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierComposition <- if _POSITION_ARG_NOM_FICHIER_COMPOSITION < argc 
                                then return ( argv !! _POSITION_ARG_NOM_FICHIER_COMPOSITION )
                                else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       nomFichierWave <- if _POSITION_ARG_NOM_FICHIER_WAVE < argc 
                         then return ( argv !! _POSITION_ARG_NOM_FICHIER_WAVE )
                         else error _MSSG_ERREUR_NOMBRE_ARGUMENT
       instruments <- readFile nomFichierInsts
       tracks <- readFile nomFichierTracks
       composition <- readFile nomFichierComposition
       ecrireWave nomFichierWave ( construireWave instruments tracks composition )

construireWave :: String -> String -> String -> [Int]
construireWave str_instruments str_tracks str_composition = (concat patrons)
      where
               pistes = parsePistes str_tracks
               instruments = parseInstruments str_instruments
               composition = parseComposition str_composition
               period = fst composition
               patrons = [ computePatron x pistes instruments period | x <- snd composition]
  

 
