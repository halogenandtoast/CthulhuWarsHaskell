{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Faction
  ( Faction(..)
  )
where

data Faction
  = GreatCthulhu
  | CrawlingChaos
  | BlackGoat
  | YellowSign
  deriving stock (Show, Eq, Ord, Enum, Bounded)
