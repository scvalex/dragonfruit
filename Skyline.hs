{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

module Skyline (
        Skyline(..), SkylineParameters(..), Building(..)
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Generics
import Graphics.Rendering.Cairo
import System.Random
import Types

data Skyline = Skyline { getBuildings :: [Building]
                       , getBackground :: RGBA }

data SkylineParameters = SkylineParameters { getMutationRate :: Double }

data Building = Building { getDimensions :: (Double, Double)
                         , getFacade :: RGBA
                         , getPositionOffset :: Double }
                deriving ( Data, Typeable )

instance Genotype Building SkylineParameters Render where
    manifest b _ = do
      uncurry4 setSourceRGBA (getFacade b)
      uncurry2 (rectangle (getPositionOffset b) 0.8) (getDimensions b)
      fill

    spawn _ = do
      let gen = liftIO (randomRIO (0.0, 1.0))
      dimensions <- (,) <$> gen <*> gen
      rgba <- (,,,) <$> gen <*> gen <*> gen <*> gen
      pos <- gen
      return $ Building dimensions rgba pos

    mutate b params = do
      let mutRate = getMutationRate params
      everywhereM (mkM (mutateDouble mutRate)) b

mutateDouble :: (MonadIO m) => Double -> Double -> m Double
mutateDouble mutRate d = do
  r <- liftIO $ randomRIO (0.0, 1.0)
  d' <- liftIO $ randomRIO (0.0, 1.0)
  return $ if r > mutRate then d else d'

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, t) = f x y z t
