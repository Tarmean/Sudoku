module HiddenSingletonPass (applyHiddenSingletons) where
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits

import Types
import WriteCell
import StreamSlice

data HiddenSingleton = None | OnlyAt !Int | Multiple

{-# INLINE applyHiddenSingletons #-}
applyHiddenSingletons ::  (PrimMonad m) => Matrix  (PrimState m) -> m Bool
applyHiddenSingletons m = allRegions hiddenSingletonPass
  where

    {-# INLINE hiddenSingletonPass #-}
    hiddenSingletonPass r =  shortCutFromTo 1 9 (step r)

    {-# INLINE step #-}
    step !r !i = do
        let !mask = toDigitSet i 
        h <- searchHiddenSingleton mask (toStream m r)
        case h of
           OnlyAt idx -> do
               writeLin m idx (mask, False)
               fixCell idx mask m
               return True
           _ -> return False

{-# INLINE searchHiddenSingleton #-}
searchHiddenSingleton :: Monad m => DigitSet -> S.Stream m (Int, DigitSet) -> m HiddenSingleton
searchHiddenSingleton !mask s = S.foldl step None s
   where
     step Multiple _ = Multiple
     step a (idx, set)
       | (set .&. mask) /= DigitSet 0 = case a of
           None -> OnlyAt idx
           _ -> Multiple
       | otherwise = a
