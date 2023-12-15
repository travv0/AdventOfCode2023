import Data.Function ((&))
import Data.List (find, foldl', minimum)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import System.IO (readFile)

data Range = Range
  { rangeSourceStart :: Integer,
    rangeDestStart :: Integer,
    rangeLength :: Integer
  }
  deriving (Show)

type ConversionMap = [Range]

ofList :: [a] -> Maybe a
ofList [] = Nothing
ofList (x : _) = Just x

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = error "List passed to makePairs had odd number of elements"
makePairs (a : b : rest) = (a, b) : makePairs rest

input :: IO String
input = readFile "input.txt"

createMap :: String -> ConversionMap
createMap text =
  let lines = tail $ splitOn "\n" text
   in map
        ( \line ->
            let parts = words line
                destStart = read (head parts) :: Integer
                sourceStart = read (parts !! 1) :: Integer
                length = read (parts !! 2) :: Integer
             in Range
                  { rangeSourceStart = sourceStart,
                    rangeDestStart = destStart,
                    rangeLength = length
                  }
        )
        lines

parseInput :: String -> ([Integer], [ConversionMap])
parseInput input =
  let parts = splitOn "\n\n" input
      seeds = map read $ words $ splitOn ": " (head parts) !! 1 :: [Integer]
   in (seeds, map createMap $ tail parts)

inRange :: Integer -> Range -> Bool
inRange seed range =
  seed >= rangeSourceStart range
    && seed < rangeSourceStart range + rangeLength range

iterateMaps :: [ConversionMap] -> Integer -> Integer
iterateMaps [] seed = seed
iterateMaps (map : restMaps) seed =
  let validRange = find (inRange seed) map
   in case validRange of
        Just range -> seed - rangeSourceStart range + rangeDestStart range
        Nothing -> seed
        & iterateMaps restMaps

findMinDistance :: Integer -> Integer -> Integer -> [ConversionMap] -> Integer
findMinDistance acc seedStart length maps =
  foldl'
    (\minDistance seed -> min minDistance $ iterateMaps maps seed)
    acc
    [seedStart .. seedStart + length - 1]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (seeds, maps) = parseInput input
  let part1 = minimum $ map (iterateMaps maps) seeds
  putStrLn $ "Part 1: " ++ show part1
