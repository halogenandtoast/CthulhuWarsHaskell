{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module CthulhuWars.Game
  ( initialGame
  , numberOfDice
  , Game(..)
  )
where

import           Data.Map.Strict         hiding (filter, map)
import           Data.Semigroup.Generic  (gmappend, gmempty)
import           GHC.Generics

import           CthulhuWars.Faction
import           CthulhuWars.FactionUnit
import           CthulhuWars.GreatOldOne
import           CthulhuWars.Maps
import           CthulhuWars.Monster
import           CthulhuWars.Player
import           CthulhuWars.Region
import           CthulhuWars.Spellbook
import           CthulhuWars.Token

newtype Gate = MkGate { gateController :: Maybe FactionUnit }
  deriving (Show, Eq)

data Piece
  = Gate Gate
  | FactionUnit FactionUnit
  | Token Token
  deriving stock (Show, Eq)

data Game = Game
  { gameMap        :: GameMap
  , gamePlayers    :: Map Faction Player
  , gamePieces     :: Map Region [Piece]
  , gameSpellbooks :: Map Faction [Spellbook]
  }
  deriving stock (Show, Generic)

instance Semigroup Game where
  (<>) = gmappend

instance Monoid Game where
  mempty = gmempty

initialGame :: Game
initialGame = mempty

ritualOfAnnihilationCost :: Game -> Int
ritualOfAnnihilationCost _ = 5

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

allPieces :: Game -> [Piece]
allPieces Game {..} = concat $ elems gamePieces

countOnMap :: Piece -> Game -> Int
countOnMap piece = count piece . allPieces

countInRegion :: Piece -> Region -> Game -> Int
countInRegion piece region Game {..} =
  count piece $ findWithDefault [] region gamePieces

allGates :: Game -> [Gate]
allGates game = [ unwrapped | Gate unwrapped <- allPieces game ]

controlledGateCount :: Faction -> Game -> Int
controlledGateCount faction = length . filter (controlledBy faction) . allGates

controlledBy :: Faction -> Gate -> Bool
controlledBy faction gate =
  maybe False (isFactionUnit faction) (gateController gate)

isFactionUnit :: Faction -> FactionUnit -> Bool
isFactionUnit faction (Acolyte controller        ) = faction == controller
isFactionUnit faction (Terror (Just controller) _) = faction == controller
isFactionUnit _       (Terror _                 _) = False
isFactionUnit faction (Monster monster           ) = case faction of
  GreatCthulhu  -> monster `elem` [DeepOne, Shoggoth, Starspawn]
  CrawlingChaos -> monster `elem` [Nightgaunt, FlyingPolyp, HuntingHorror]
  BlackGoat     -> monster `elem` [Ghoul, FungiFromYuggoth, DarkYoung]
  YellowSign    -> monster `elem` [Undead, Byakhee]
isFactionUnit faction (GreatOldOne greatOldOne) = case faction of
  GreatCthulhu  -> greatOldOne == Cthulhu
  CrawlingChaos -> greatOldOne == Nyarlathotep
  BlackGoat     -> greatOldOne == ShubNiggurath
  YellowSign    -> greatOldOne `elem` [KingInYellow, Hastur]

numberOfDice :: Faction -> Faction -> Region -> Game -> Int
numberOfDice faction enemy region game@Game {..} = case faction of
  GreatCthulhu ->
    monsterCount DeepOne
      + (monsterCount Shoggoth * 2)
      + (monsterCount Starspawn * 3)
      + (greatOldOneCount Cthulhu * 6)
  CrawlingChaos ->
    monsterCount FlyingPolyp
      + (monsterCount HuntingHorror * 2)
      + ( greatOldOneCount Nyarlathotep
        * (length (spellbooks CrawlingChaos) + length (spellbooks enemy))
        )
  BlackGoat ->
    (if Frenzy `elem` spellbooks BlackGoat then acolyteCount BlackGoat else 0)
      + monsterCount FungiFromYuggoth
      + (monsterCount DarkYoung * 2)
      + ( greatOldOneCount ShubNiggurath
        * ( countOnMap (FactionUnit (Acolyte BlackGoat)) game
          + controlledGateCount BlackGoat game
          + (if TheRedSign `elem` spellbooks BlackGoat
              then countOnMap (FactionUnit (Monster DarkYoung)) game
              else 0
            )
          )
        )
  YellowSign ->
    max 0 (countInRegion (FactionUnit (Monster Undead)) region game - 1)
      + (if countInRegion (FactionUnit (Monster Byakhee)) region game > 0
          then 1 + countInRegion (FactionUnit (Monster Byakhee)) region game
          else 0
        )
      + (greatOldOneCount Hastur * ritualOfAnnihilationCost game)
 where
  pieces = findWithDefault [] region gamePieces
  spellbooks faction' = findWithDefault [] faction' gameSpellbooks
  unitCount        = flip count pieces . FactionUnit
  acolyteCount     = unitCount . Acolyte
  monsterCount     = unitCount . Monster
  greatOldOneCount = unitCount . GreatOldOne
