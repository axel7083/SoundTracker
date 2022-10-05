module DataSet (pistes, instruments, patrons) where

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
    "piste 0" -- index 0
  , "silence"
  , "-"
  , "A 1 3.0"
  , "-"
  , "piste 1" -- index 1
  , "B 1 3.0"
  , "-"
  , "-"
  , "-"
  , "piste 2" -- index 2
  , "silence"
  , "silence"
  , "silence"
  ]

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