{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Player
  ( Player(..)
  , PlayerType(..)
  )
where

data PlayerType = Human | Bot
  deriving (Show, Eq)

data Player = Player
  { playerType  :: PlayerType
  , playerPower :: Int
  } deriving stock (Show)
