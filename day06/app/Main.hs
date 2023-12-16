import Data.List.Split (splitOn)

data Race = Race {raceTime :: Integer, recordDist :: Integer} deriving (Show)

parseInputPart1 :: [String] -> [Race]
parseInputPart1 [timeLine, distLine] =
  let parseLine = map read . tail . filter (/= "") . splitOn " "
      times = parseLine timeLine
      dists = parseLine distLine
   in zipWith Race times dists
parseInputPart1 input = error $ "Invalid input: " ++ show input

parseInputPart2 :: [String] -> Race
parseInputPart2 [timeLine, distLine] =
  let parseLine = read . concat . tail . filter (/= "") . splitOn " "
      time = parseLine timeLine
      dist = parseLine distLine
   in Race time dist
parseInputPart2 input = error $ "Invalid input: " ++ show input

calculateWinningTimes :: Race -> [Integer]
calculateWinningTimes race =
  filter
    (\heldTime -> heldTime * (raceTime race - heldTime) > recordDist race)
    [1 .. raceTime race - 1]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  let winningTimes1 = map calculateWinningTimes $ parseInputPart1 input
  putStrLn $ "Part 1: " ++ show (product . map length $ winningTimes1)

  let winningTimes2 = calculateWinningTimes $ parseInputPart2 input
  putStrLn $ "Part 2: " ++ show (length winningTimes2)
