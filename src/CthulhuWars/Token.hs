{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Token
  ( Token(..)
  )
where

data Token = Desecration
  deriving stock (Show, Eq)
