module CthulhuWars.GameSpec
  ( spec
  )
where

import           CthulhuWars.Game
import           Test.Hspec
import           Test.QuickCheck

-- brittany --exactprint-only

spec :: Spec
spec = describe "Game" $
  it "has no players to start with" $
    gamePlayers initialGame `shouldBe` mempty
  it "has an empty map to start with" $
    gameMap initialGame `shouldBe` mempty
