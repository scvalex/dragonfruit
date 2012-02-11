{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

module Skyline (
        Skyline(..), SkylineParameters(..), Building(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics
import Graphics.Rendering.Cairo
import System.Random
import Types

data Skyline = Skyline { getBuildings :: [Building]
                       , getBackground :: RGBA }

instance Genotype Skyline SkylineParameters Render where
    visualize params sky = do
      uncurry4 setSourceRGBA (getBackground sky)
      paint
      forM_ (getBuildings sky) (visualize params)

    spawn params = do
      let genD = liftIO (randomRIO (0.1, 1.0))
      rgba <- (,,,) <$> genD <*> genD <*> genD <*> genD
      bs <- replicateM 3 (spawn params)
      return $ Skyline bs rgba

    mutate params sky = do
      let mutRate = getMutationRate params
      bs' <- mapM (mutate params) (getBuildings sky)
      rgba' <- everywhereM (mkM (mutateDouble mutRate)) (getBackground sky)
      return sky { getBuildings = bs', getBackground = rgba' }

data SkylineParameters = SkylineParameters { getMutationRate :: Double }

data Building = Building { getDimensions :: (Double, Double)
                         , getFacade :: RGBA
                         , getPositionOffset :: Double }
                deriving ( Data, Typeable )

instance Genotype Building SkylineParameters Render where
    visualize _ b = do
      uncurry4 setSourceRGBA (getFacade b)
      uncurry2 (rectangle (getPositionOffset b) 0.8) (getDimensions b)
      fill

    spawn _ = do
      let gen = liftIO (randomRIO (0.1, 1.0))
      dimensions <- (,) <$> gen <*> gen
      rgba <- (,,,) <$> gen <*> gen <*> gen <*> gen
      pos <- gen
      return $ Building dimensions rgba pos

    mutate params b = do
      let mutRate = getMutationRate params
      everywhereM (mkM (mutateDouble mutRate)) b

mutateDouble :: (MonadIO m) => Double -> Double -> m Double
mutateDouble mutRate d = do
  r <- liftIO $ randomRIO (0.0, 1.0)
  d' <- liftIO $ randomRIO (0.1, 1.0)
  return $ if r > mutRate then d else d'

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, t) = f x y z t
