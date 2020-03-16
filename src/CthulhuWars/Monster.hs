{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Monster
  ( Monster(..)
  )
where

data Monster
  = DeepOne
  | Shoggoth
  | Starspawn
  | Nightgaunt
  | FlyingPolyp
  | HuntingHorror
  | Ghoul
  | FungiFromYuggoth
  | DarkYoung
  | Undead
  | Byakhee
  deriving stock (Show, Eq)
