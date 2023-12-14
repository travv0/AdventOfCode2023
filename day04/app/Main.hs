{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Foldable (foldl')
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepEndBy)
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer qualified as L

data Scratchcard = Scratchcard
  { number :: Int,
    yourNumbers :: Set Int,
    winningNumbers :: Set Int,
    matchingNumberCount :: Int,
    cardCount :: Int
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let cards = Map.fromList $ map parseLine $ lines input
      part1 = sum $ map calculateScore $ Map.toList cards
  putStrLn $ "Part 1: " ++ show part1
  let cards' = foldl' updateCounts cards $ Map.toList cards
      part2 = sum $ map (cardCount . snd) $ Map.toList cards'
  putStrLn $ "Part 2: " ++ show part2

type Parser = Parsec Void String

parseLine :: String -> (Int, Scratchcard)
parseLine line =
  case parse cardP "" line of
    Right card -> card
    Left err -> error $ "Failed to parse line: " ++ errorBundlePretty err

cardP :: Parser (Int, Scratchcard)
cardP = do
  number <- string "Card" *> space *> L.decimal <* string ": "
  numbersSections <- numbersSectionsP
  let yourNumbers = Set.fromList $ fst numbersSections
      winningNumbers = Set.fromList $ snd numbersSections
      matchingNumbers = Set.intersection yourNumbers winningNumbers
  return
    ( number,
      Scratchcard
        { number,
          yourNumbers,
          winningNumbers,
          matchingNumberCount = Set.size matchingNumbers,
          cardCount = 1
        }
    )

numbersSectionsP :: Parser ([Int], [Int])
numbersSectionsP = do
  yourNumbers <- space *> numbersSectionP <* char '|' <* space
  winningNumbers <- space *> numbersSectionP
  return (yourNumbers, winningNumbers)

numbersSectionP :: Parser [Int]
numbersSectionP = L.decimal `sepEndBy` space

calculateScore :: (Int, Scratchcard) -> Int
calculateScore (_, Scratchcard {matchingNumberCount = 0}) = 0
calculateScore (_, Scratchcard {matchingNumberCount = n}) = 2 ^ (n - 1)

updateCounts :: Map Int Scratchcard -> (Int, Scratchcard) -> Map Int Scratchcard
updateCounts cards (number, Scratchcard {matchingNumberCount = matches}) =
  foldl'
    ( \cards' i ->
        let card = cards' ! number
            iCard = cards' ! i
         in Map.insert
              i
              (iCard {cardCount = cardCount iCard + cardCount card})
              cards'
    )
    cards
    [number + 1 .. number + matches]