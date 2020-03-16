{-# LANGUAGE DerivingStrategies #-}

module CthulhuWars.FactionUnit
  ( FactionUnit(..)
  )
where

import           CthulhuWars.Faction
import           CthulhuWars.GreatOldOne
import           CthulhuWars.Monster
import           CthulhuWars.Terror

data FactionUnit
  = Acolyte Faction
  | Monster Monster
  | GreatOldOne GreatOldOne
  | Terror (Maybe Faction) Terror
  deriving stock (Show, Eq)
