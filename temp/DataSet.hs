module DataSet (pistes, instruments, patrons) where

-- Here is the value than must be at the first line of our order.txt
durationInstruction :: Double
durationInstruction = 2.0  
  
patrons :: [String]
patrons = [
   "0"
  ,"0"
  ,"1"
  ,"1"
  ,"2"
  ,"2"
  ,"3"
  ,"3"
  ]  
  
pistes :: [String]
pistes = [
 "piste 0" --8
 ,"C 4 0.33"
 ,"-"
 ,"D 4 0.33"
 ,"-"
 ,"E 4 0.33"
 ,"-"
 ,"C 4 0.33"
 ,"-"
 ,"piste 0" --8
 ,"E 4 0.33"
 ,"-"
 ,"F 4 0.33"
 ,"-"
 ,"G 4 0.33"
 ,"-"
 ,"-"
 ,"-"
 ,"piste 0" --8
 ,"G 4 0.33"
 ,"A 4 0.33"
 ,"G 4 0.33"
 ,"F 4 0.33"
 ,"E 4 0.33"
 ,"-"
 ,"D 4 0.33"
 ,"-"
 ,"piste 0" -- 8
 ,"D 4 0.33"
 ,"-"
 ,"G 4 0.33"
 ,"-"
 ,"C 4 0.33"
 ,"-"
 ,"-"
 ,"-"
  ]

instruments :: [String]
instruments = [
      "instrument"
    , "sin 0.0 0.0 1.0 0.0" -- e a1 a2 phi
  ]