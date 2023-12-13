import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

replaceDigitWords :: String -> String
replaceDigitWords "" = ""
replaceDigitWords line@(c : rest) =
  let newDigit
        | "one" `isPrefixOf` line = "1"
        | "two" `isPrefixOf` line = "2"
        | "three" `isPrefixOf` line = "3"
        | "four" `isPrefixOf` line = "4"
        | "five" `isPrefixOf` line = "5"
        | "six" `isPrefixOf` line = "6"
        | "seven" `isPrefixOf` line = "7"
        | "eight" `isPrefixOf` line = "8"
        | "nine" `isPrefixOf` line = "9"
        | isDigit c = [c]
        | otherwise = ""
   in newDigit ++ replaceDigitWords rest

getCalibrationValue :: String -> Maybe Int
getCalibrationValue line = do
  firstDigit <- find isDigit line
  lastDigit <- find isDigit (reverse line)
  readMaybe [firstDigit, lastDigit]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let linesOfFile = lines input

  let part1 = sum $ mapMaybe getCalibrationValue linesOfFile
  putStrLn $ "Part 1: " ++ show part1

  let part2 =
        sum $ mapMaybe (getCalibrationValue . replaceDigitWords) linesOfFile
  putStrLn $ "Part 2: " ++ show part2