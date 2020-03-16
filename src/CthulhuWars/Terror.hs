{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Terror
  ( Terror(..)
  )
where

data Terror = TheThing
  deriving stock (Show, Eq)
