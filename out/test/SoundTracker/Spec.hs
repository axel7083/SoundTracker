main :: IO ()
main = do
   contents <- readFile "./test/instruments.txt"
   print $ lines contents




