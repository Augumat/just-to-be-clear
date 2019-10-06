import System.IO
--import Data.List.Split

-- Main function
main = do
  -- Open the input file and retrieve its contents
  inputFile <- openFile "./in/in.cpp" ReadMode
  contents <- hGetContents inputFile

  -- [DEBUG] Print the file to the console
  --putStr contents

  -- Split the contents of the file by lines first
  putStr (show (nMasks 10))

  -- Write the new output to the output file
  writeFile "./out/out.cpp" contents
  hClose inputFile



-- Binary 'E' mask generation
nMasks :: Int -> [String]
nMasks n = map (("y"++) . (++"t")) (map toBin [1..n])

toBin :: Int -> String
toBin n = map eIfy (reverse (binHelp n))

eIfy :: Int -> Char
eIfy c
  | c == 0    = 'e'
  | c == 1    = 'E'
  | otherwise = '-'

binHelp :: Int -> [Int]
binHelp 0 = []
binHelp n = (n `mod` 2) : binHelp (n `div` 2)
