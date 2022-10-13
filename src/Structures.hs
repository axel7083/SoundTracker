module Structures (
Group ( Group ), index, members, title,
Note ( Note ), frequence, volume, 
Instrument ( Instrument ), instrumentId, ondes,
Instruction ( Instruction ), duration, note,
Piste ( Piste ), pisteId, instructions, instrument,
FParam ( FParam ), e, a1, a2, q, phi) where

-- ==================================================================
-- Structure used in the program
-- We make the members strict to optimize the program
-- ==================================================================  
  
data Piste = Piste {
    pisteId :: !Int  
  , instrument :: !Int -- instrument id (index)
  , instructions :: ![Instruction]
} deriving (Show)

data Instruction = Instruction
  {
      duration :: !Int -- number of "-" + 1
  --,   instrument :: !Int -- instrument id (index)
  ,   note :: !Note -- the note
  } deriving (Show)

data Note = Note
  {
      frequence :: !Double
  ,   volume    :: !Double
  } deriving (Show)
  
data Instrument = Instrument
    {
      instrumentId :: !Int
    , ondes :: ![Double -> Double -> Double] -- args: f t => double
    } 

-- When parsing a file we need to group the content
data Group = Group
  {
    index :: Int
  , members :: [String]
  , title :: String
  } deriving (Show)
  
data FParam = FParam
  {
    e :: Double
  , a1 :: Double
  , a2 :: Double
  , q :: Double -- this will be equal to 0 when used for the sinus
  , phi :: Double
  }