{-# LANGUAGE TypeApplications #-}
module Main where

import           CthulhuWars.Faction
import           CthulhuWars.Game    (Game (..), initialGame)
import           CthulhuWars.Player
import           Data.List           (intercalate)
import           Data.Map.Strict     hiding (map)
import           System.IO           (hFlush, stdout)
import           Text.Read           (readMaybe)

runGame :: Game -> IO ()
runGame = undefined

for :: [a] -> (a -> b) -> [b]
for = flip map

pickInRange :: String -> (Int, Int) -> IO Int
pickInRange prompt (x, y) = go
 where
  go = do
    putStr prompt
    hFlush stdout
    choice <- readMaybe <$> getLine
    case choice of
      Just c  -> if c >= x && c <= y then pure c else go
      Nothing -> go

pickFactions :: IO (Map Faction Player)
pickFactions = do
  player <- toEnum <$> pickInRange prompt factionRange
  pure . fromList $ for allFactions $ \faction ->
    (faction, make (faction == player))
 where
  prompt       = displayFactions ++ "\n" ++ "Pick a faction: "
  factionRange = (1, fromEnum (maxBound @Faction) + 1)
  make isPlayer =
    Player { playerType = if isPlayer then Human else Bot, playerPower = 8 }

allFactions :: [Faction]
allFactions = [minBound .. maxBound]

displayFactions :: String
displayFactions =
  intercalate "\n" $ for (zip [1 :: Int ..] allFactions) $ \(i, f) ->
    show i ++ ": " ++ show f

main :: IO ()
main = do
  factions <- pickFactions
  runGame $ initialGame { gamePlayers = factions }
