{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CthulhuWars.Maps
  ( earth35
  , GameMap(..)
  )
where

import           Data.Map

import           CthulhuWars.Region

newtype GameMap = GameMap
  { getMap :: Map Region [Region]
  } deriving newtype (Show, Semigroup, Monoid)

earth35 :: GameMap
earth35 =
  GameMap $ fromList [(Antarctica, [Australia]), (Australia, [Antarctica])]

