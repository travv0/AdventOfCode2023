{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (isAlpha)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

data Dir = L | R deriving (Eq, Show)

data Node = Node
  { label :: String,
    left :: String,
    right :: String
  }
  deriving (Eq, Show)

parseNode :: String -> Node
parseNode s =
  Node
    { label = head parts,
      left = filter isAlpha (head children),
      right = filter isAlpha (children !! 1)
    }
  where
    parts = splitOn " = " s
    children = splitOn ", " (parts !! 1)

makeGraph :: [Node] -> Map String Node
makeGraph = foldr (\node -> Map.insert (label node) node) Map.empty

parseInput :: [String] -> ([Dir], Map String Node)
parseInput ls = (dirs, makeGraph nodes)
  where
    dirLine = head ls
    dirs =
      map
        ( \case
            'L' -> L
            'R' -> R
            _ -> error "invalid input"
        )
        dirLine
    nodes = map parseNode (drop 2 ls)

run :: Map String Node -> [Dir] -> String -> Int
run graph dirs nodeLabel = run' (cycle dirs) nodeLabel 0
  where
    run' [] _ _ = undefined
    run' (h : t) nodeLabel' count
      | nodeLabel' == "ZZZ" = count
      | otherwise =
          let Node {left, right} = graph ! nodeLabel'
           in run'
                t
                ( case h of
                    L -> left
                    R -> right
                )
                (count + 1)

runParallel :: Map String Node -> [Dir] -> [String] -> Int
runParallel graph dirs nodeLabels = run' (cycle dirs) nodeLabels 0
  where
    run' [] _ _ = undefined
    run' (h : t) nodeLabels' count
      | all (isSuffixOf "Z") nodeLabels' = count
      | otherwise =
          let nextLabels =
                map
                  ( \nodeLabel ->
                      let Node {left, right} = graph ! nodeLabel
                       in case h of
                            L -> left
                            R -> right
                  )
                  nodeLabels'
           in run'
                t
                nextLabels
                (count + 1)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let (dirs, graph) = parseInput input
  putStrLn $ "Part 1: " ++ show (run graph dirs "AAA")
  let startLabels = filter (\label -> "A" `isSuffixOf` label) $ Map.keys graph
  putStrLn $ "Part 2: " ++ show (runParallel graph dirs startLabels)