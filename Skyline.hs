{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

module Skyline (
        Skyline(..), SkylineParameters(..), Building(..)
    ) where

import Common
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics
import Graphics.Rendering.Cairo
import Types

data Skyline = Skyline { getBuildings :: [Building]
                       , getBackground :: RGBA }

instance Genotype Skyline SkylineParameters Render where
    visualize params sky = do
      uncurry4 setSourceRGBA (getBackground sky)
      paint
      forM_ (getBuildings sky) (visualize params)

    spawn params = do
      let genD = randomDouble 0.1 1.0
      rgba <- (,,,) <$> genD <*> genD <*> genD <*> genD
      bs <- replicateM 30 (spawn params)
      return $ Skyline bs rgba

    mutate params sky = do
      let mutRate = getMutationRate params
      bs' <- mapM (mutate params) (getBuildings sky)
      rgba' <- everywhereM (mkM (mutateDouble mutRate)) (getBackground sky)
      r <- randomDouble 0.0 1.0
      bs'' <- if r < mutRate then (:) <$> spawn params <*> return bs'
                             else return bs'
      return sky { getBuildings = bs'', getBackground = rgba' }

data SkylineParameters = SkylineParameters { getMutationRate :: Double }

data Building = Building { getDimensions :: (Double, Double)
                         , getFacade :: RGBA
                         , getPositionOffset :: Double }
                deriving ( Data, Typeable )

instance Genotype Building SkylineParameters Render where
    visualize _ b = do
      uncurry4 setSourceRGBA (getFacade b)
      uncurry2 (rectangle (getPositionOffset b) 0.2) (getDimensions b)
      fill

    spawn _ = do
      let randomBuilding = do
                let gen = randomDouble 0.1 1.0
                dims <- (,) <$> gen <*> gen
                rgba <- (,,,) <$> gen <*> gen <*> gen <*> gen
                pos <- randomDouble 0.1 7.9
                return $ Building dims rgba pos
      constrainedRandom validBuilding randomBuilding

    mutate params b = do
      let mutRate = getMutationRate params
      everywhereM (mkM (mutateDouble mutRate)) b

validBuilding :: Building -> Bool
validBuilding Building {getDimensions = (w, h), getPositionOffset = pos} =
    w > 0 && h > 0 && w + pos > 0 && w + pos <= 8.0 && h <= 0.8 && w < 2.0

mutateDouble :: (MonadIO m) => Double -> Double -> m Double
mutateDouble mutRate d = do
  r <- randomDouble 0.0 1.0
  dif <- randomDouble (-0.2) 0.2
  return $ if r > mutRate then d else d + dif

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, t) = f x y z t
