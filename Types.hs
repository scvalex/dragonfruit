{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Types (
        Genotype(..), RGBA
    ) where

import Control.Monad.IO.Class

class (Monad f) => Genotype g p f | g -> f where
    realize :: g -> p -> f ()
    spawn :: (MonadIO m) => p -> m g

type RGBA = (Double, Double, Double, Double)
