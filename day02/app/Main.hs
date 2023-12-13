{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    errorBundlePretty,
    parse,
    sepBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (digitChar, newline, string)

type Parser = Parsec Void String

data Color = Blue | Green | Red deriving (Show, Ord, Eq)

type Set = Map.Map Color Int

data Game = Game {gameId :: Int, gameSets :: [Set]}

gameP :: Parser Game
gameP = do
  gameId <- string "Game " *> some digitChar <* string ": "
  sets <- sepBy1 setP (string "; ")
  return $ Game (read gameId) sets

setP :: Parser Set
setP = do
  counts <- sepBy1 countP (string ", ")
  return (Map.fromList counts)

countP :: Parser (Color, Int)
countP = do
  count <- some digitChar <* string " "
  color <- colorP
  return (color, read count)

colorP :: Parser Color
colorP =
  (Blue <$ string "blue")
    <|> (Green <$ string "green")
    <|> (Red <$ string "red")

numberOfColor :: Color -> Set -> Int
numberOfColor color set = fromMaybe 0 (Map.lookup color set)

gameImpossible :: Game -> Bool
gameImpossible (Game {gameSets}) =
  any
    ( \set ->
        numberOfColor Red set > 12
          || numberOfColor Green set > 13
          || numberOfColor Blue set > 14
    )
    gameSets

fewestCubesForGame :: Game -> (Int, Int, Int)
fewestCubesForGame (Game {gameSets}) = foldr1 maxCubes counts
  where
    counts =
      fmap
        ( \set ->
            ( numberOfColor Red set,
              numberOfColor Blue set,
              numberOfColor Green set
            )
        )
        gameSets
    maxCubes (r1, b1, g1) (r2, b2, g2) = (max r1 r2, max b1 b2, max g1 g2)

parseGames :: String -> Either (ParseErrorBundle String Void) [Game]
parseGames = parse (some $ gameP <* newline) ""

main :: IO ()
main = do
  input <- readFile "input.txt"
  case parseGames input of
    Left errorBundle -> putStrLn $ errorBundlePretty errorBundle
    Right games -> do
      putStrLn $
        "Part 1: "
          ++ show
            ( sum $
                map gameId $
                  filter (not . gameImpossible) games
            )
      putStrLn $
        "Part 2: "
          ++ show
            ( sum $ map ((\(r, g, b) -> r * g * b) . fewestCubesForGame) games
            )
