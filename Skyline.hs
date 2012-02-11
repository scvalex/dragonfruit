{-# LANGUAGE MultiParamTypeClasses #-}

module Skyline (
        Skyline(..), Building(..)
    ) where

import Graphics.Rendering.Cairo
import Types

data Building = Building { getDimensions :: (Double, Double)
                         , getFacade :: RGBA
                         , getPositionOffset :: Double }

instance Genotype Building () Render where
    realize b _ = do
      uncurry4 setSourceRGBA (getFacade b)
      let (w, h) = getDimensions b
      rectangle (getPositionOffset b) 0.8 w h
      fill

    spawn _ = return $ Building (0, 0) (0, 0, 0, 0) 0

data Skyline = Skyline { getBuildings :: [Building]
                       , getBackground :: RGBA }

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, t) = f x y z t
