{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector

data Symbol = Symbol {symbol :: Char, symbolX :: Int, symbolY :: Int}
  deriving (Show, Eq, Ord)

type PartNumbers = Map Symbol [Int]

type Input = Vector (Vector Char)

makePartNumberMap :: Input -> PartNumbers
makePartNumberMap input = runST $ do
  partNumbersRef <- newSTRef Map.empty
  let (height, width) = (length input, length (Vector.head input))

  forM_ [0 .. height - 1] $ \y -> do
    numberRef <- newSTRef ""
    adjacentSymbolRef <- newSTRef Nothing

    forM_ [0 .. width - 1] $ \x -> do
      let c = input ! y ! x

      when (isDigit c) $ do
        modifySTRef' numberRef (++ [c])

        case getAdjacentSymbol x y input of
          Just sym -> writeSTRef adjacentSymbolRef (Just sym)
          Nothing -> return ()

      number <- readSTRef numberRef
      when (not (null number) && not (isDigit c)) $ do
        adjacentSymbol <- readSTRef adjacentSymbolRef
        partNumbers <- readSTRef partNumbersRef

        case adjacentSymbol of
          Just sym -> do
            let pns = Map.lookup sym partNumbers
            let updatedPns = case pns of
                  Just numbers -> read (number :: String) : numbers
                  Nothing -> [read number :: Int]
            writeSTRef partNumbersRef $ Map.insert sym updatedPns partNumbers
          Nothing -> return ()

        writeSTRef numberRef ""
        writeSTRef adjacentSymbolRef Nothing
  readSTRef partNumbersRef

getAdjacentSymbol :: Int -> Int -> Input -> Maybe Symbol
getAdjacentSymbol x y input
  | y > 0 && isSymbol (input ! (y - 1) ! x) =
      Just $ Symbol (input ! (y - 1) ! x) x (y - 1)
  | y < height - 1 && isSymbol (input ! (y + 1) ! x) =
      Just $ Symbol (input ! (y + 1) ! x) x (y + 1)
  | x > 0 && isSymbol (input ! y ! (x - 1)) =
      Just $ Symbol (input ! y ! (x - 1)) (x - 1) y
  | x < width - 1 && isSymbol (input ! y ! (x + 1)) =
      Just $ Symbol (input ! y ! (x + 1)) (x + 1) y
  | y > 0 && x > 0 && isSymbol (input ! (y - 1) ! (x - 1)) =
      Just $ Symbol (input ! (y - 1) ! (x - 1)) (x - 1) (y - 1)
  | y < height - 1 && x > 0 && isSymbol (input ! (y + 1) ! (x - 1)) =
      Just $ Symbol (input ! (y + 1) ! (x - 1)) (x - 1) (y + 1)
  | y > 0 && x < width - 1 && isSymbol (input ! (y - 1) ! (x + 1)) =
      Just $ Symbol (input ! (y - 1) ! (x + 1)) (x + 1) (y - 1)
  | y < height - 1 && x < width - 1 && isSymbol (input ! (y + 1) ! (x + 1)) =
      Just $ Symbol (input ! (y + 1) ! (x + 1)) (x + 1) (y + 1)
  | otherwise = Nothing
  where
    isSymbol c = not (isDigit c) && c /= '.'
    width = length (Vector.head input)
    height = length input

main :: IO ()
main = do
  input <-
    Vector.fromList . map (Vector.fromList . (++ ".")) . lines
      <$> readFile "input.txt"

  let partNumbers = makePartNumberMap input

  let part1 = sum (concatMap snd (Map.toList partNumbers))
  putStrLn $ "Part 1: " ++ show part1

  let part2 =
        sum
          [ num1 * num2
            | sym <- Map.keys partNumbers,
              symbol sym == '*',
              [num1, num2] <- maybeToList (Map.lookup sym partNumbers)
          ]
  putStrLn $ "Part 2: " ++ show part2