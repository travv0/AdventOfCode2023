{-# LANGUAGE MultiWayIf #-}

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (find, foldl', group, nub, sort, sortBy)

data Hand = Hand {cards :: String, bid :: Int} deriving (Show)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

parseInput :: [String] -> [Hand]
parseInput =
  map
    ( \line ->
        let parts = words line
         in Hand (head parts) (read (parts !! 1) :: Int)
    )

replaceJokers :: String -> [String]
replaceJokers hand =
  let uniqueCards = nub hand
      replacementsForJokers =
        map (\card -> if card == 'J' then uniqueCards else [card]) hand
      generatePossibleHands hands cardOptions =
        [hand ++ [card] | hand <- hands, card <- cardOptions]
   in foldl' generatePossibleHands [""] replacementsForJokers

classifyHand :: String -> HandType
classifyHand hand =
  let numRank n = any ((== n) . length)
      countRanks n r = length $ filter ((== n) . length) r
      ranks = group $ sort hand
   in if
        | numRank 5 ranks -> FiveOfAKind
        | numRank 4 ranks -> FourOfAKind
        | numRank 3 ranks && numRank 2 ranks -> FullHouse
        | numRank 3 ranks -> ThreeOfAKind
        | countRanks 2 ranks == 2 -> TwoPair
        | numRank 2 ranks -> OnePair
        | otherwise -> HighCard

classifyHandWithJokers :: String -> HandType
classifyHandWithJokers hand = maximum $ map classifyHand (replaceJokers hand)

cardToValue :: Int -> Char -> Int
cardToValue jValue card = case card of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> jValue
  'T' -> 10
  n -> digitToInt n

cardComparer :: Int -> Char -> Char -> Ordering
cardComparer jValue card1 card2 =
  compare (cardToValue jValue card1) (cardToValue jValue card2)

handComparer :: (String -> HandType) -> Int -> Hand -> Hand -> Ordering
handComparer classifyFn jValue hand1 hand2 =
  let hand1Type = classifyFn (cards hand1)
      hand2Type = classifyFn (cards hand2)
   in case compare hand1Type hand2Type of
        EQ ->
          maybe
            EQ
            (uncurry (cardComparer jValue))
            (find (uncurry (/=)) $ zip (cards hand1) (cards hand2))
        o -> o

main :: IO ()
main = do
  input <- readFile "input.txt"
  let hands = parseInput $ lines input

  hands
    & sortBy (handComparer classifyHand 11)
    & zipWith (\rank hand -> rank * bid hand) [1 ..]
    & sum
    & (("Part 1: " ++) . show)
    & putStrLn

  hands
    & sortBy (handComparer classifyHandWithJokers 1)
    & zipWith (\rank hand -> rank * bid hand) [1 ..]
    & sum
    & (("Part 2: " ++) . show)
    & putStrLn
