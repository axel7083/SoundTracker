module Structs (
Group ( Group ), index, members,
Note ( Note ), frequence, volume, 
Instrument ( Instrument ), instrumentId, ondes,
Instruction ( Instruction ), duration, instrument, note,
Piste ( Piste ), pisteId, instructions) where

  
data Piste = Piste {
    pisteId :: Int  
  , instructions :: [Instruction]
}

data Instruction = Instruction
  {
      duration :: Int -- number of "-" + 1
  ,   instrument :: Int -- instrument id (index)
  ,   note :: Note -- the note
  }

data Note = Note
  {
      frequence :: Double
  ,   volume    :: Double
  }
  
data Instrument = Instrument
    {
      instrumentId :: Int
    , ondes :: [Double -> Double -> Double] -- args: f t => double
    }  

-- When parsing a file we need to group the content
data Group = Group
  {
    index :: Int
  , members :: [String]
  }