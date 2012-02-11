{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances (
    ) where

import Control.Applicative
import Graphics.Rendering.Cairo

instance Applicative Render where
    pure = return
    u <*> v  = do
      f <- u
      x <- v
      return $ f x
