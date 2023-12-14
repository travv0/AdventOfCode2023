{-# LANGUAGE ImportQualifiedPost #-}

import Data.Map qualified as Map
import Lib
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "Parsers" $ do
    it "parses a color" $ do
      parse colorP "" "blue" `shouldBe` Right Blue
      parse colorP "" "green" `shouldBe` Right Green
      parse colorP "" "red" `shouldBe` Right Red

    it "parses a count" $ do
      parse countP "" "5 blue" `shouldBe` Right (Blue, 5)
      parse countP "" "3 green" `shouldBe` Right (Green, 3)
      parse countP "" "7 red" `shouldBe` Right (Red, 7)

    it "parses a set" $ do
      let expectedSet = Map.fromList [(Blue, 5), (Green, 3), (Red, 7)]
      parse setP "" "5 blue, 3 green, 7 red" `shouldBe` Right expectedSet

    it "parses a game" $ do
      let gameStr = "Game 1: 5 blue, 3 green, 7 red; 4 blue, 2 green, 6 red"
      let expectedGame =
            Game
              1
              [ Map.fromList [(Blue, 5), (Green, 3), (Red, 7)],
                Map.fromList [(Blue, 4), (Green, 2), (Red, 6)]
              ]
      parse gameP "" gameStr `shouldBe` Right expectedGame

  describe "Game logic" $ do
    it "calculates the number of a specific color in a set" $ do
      let set = Map.fromList [(Blue, 5), (Green, 3), (Red, 7)]
      numberOfColor Blue set `shouldBe` 5
      numberOfColor Green set `shouldBe` 3
      numberOfColor Red set `shouldBe` 7

    it "checks if a game is impossible" $ do
      let game =
            Game
              1
              [ Map.fromList [(Blue, 15), (Green, 3), (Red, 7)],
                Map.fromList [(Blue, 4), (Green, 2), (Red, 6)]
              ]
      gameImpossible game `shouldBe` True

    it "calculates the fewest cubes for a game" $ do
      let game =
            Game
              1
              [ Map.fromList [(Blue, 5), (Green, 3), (Red, 7)],
                Map.fromList [(Blue, 4), (Green, 2), (Red, 6)]
              ]
      fewestCubesForGame game `shouldBe` (7, 5, 3)