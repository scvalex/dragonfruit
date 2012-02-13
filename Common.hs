module Common where

import Control.Monad.IO.Class
import System.Random

debug :: Bool
debug = False

randomDouble :: (MonadIO m) => Double -> Double -> m Double
randomDouble lo up = liftIO $ randomRIO (lo, up)

constrainedRandom :: (MonadIO m) => (a -> Bool) -> m a -> m a
constrainedRandom cond gen = go (10 :: Int)
    where
      go 0 = do
        liftIO $ putStrLn "WARNING: constrained random limit reached"
        gen
      go n = do
        x <- gen
        if cond x then return x else go (n-1)
