main :: IO ()
main = do
   print $ runTests tests 0
   


   
tests :: [Bool]
tests = [
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