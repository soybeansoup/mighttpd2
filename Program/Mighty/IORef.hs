{-# LANGUAGE BangPatterns #-}

module Program.Mighty.IORef where

import Data.IORef

----------------------------------------------------------------

-- | Strict version of 'atomicModifyIORef'
--   The returned value is strictly evaluated and thrown away.
--   This prevents space leak.
strictAtomicModifyIORef :: IORef a -> (a -> a) -> IO ()
strictAtomicModifyIORef ref f = do
    !_ <- atomicModifyIORef ref (\x -> let !r = f x in (r, ()))
    return ()
