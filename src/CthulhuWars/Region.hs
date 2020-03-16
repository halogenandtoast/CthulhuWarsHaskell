{-# LANGUAGE DerivingStrategies #-}
module CthulhuWars.Region
  ( Region(..)
  )
where

data Region = Antarctica | Australia
  deriving stock (Show, Eq, Ord)
