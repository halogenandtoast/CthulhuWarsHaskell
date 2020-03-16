{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.GreatOldOne
  ( GreatOldOne(..)
  )
where

data GreatOldOne
  = Cthulhu
  | Nyarlathotep
  | ShubNiggurath
  | KingInYellow
  | Hastur
  deriving stock (Show, Eq)
