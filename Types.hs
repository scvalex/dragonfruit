{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Types (
        Genotype(..), RGBA
    ) where

import Control.Applicative
import Control.Monad.IO.Class

class (Monad f) => Genotype g p f | g -> f where
    visualize :: g -> p -> f ()
    spawn :: (MonadIO m, Applicative m) => p -> m g
    mutate :: (MonadIO m, Applicative m) => g -> p -> m g

type RGBA = (Double, Double, Double, Double)
