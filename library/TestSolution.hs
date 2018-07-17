module TestSolution where


import Data.Bits
import Types
import Shape
import StreamSlice
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Trace


-- {-# INLINE checkComplete #-}
checkComplete :: Matrix (PrimState IO) -> IO Bool
checkComplete m = allRegions pass
  where
    pass = fmap check . S.foldl' step (Just (DigitSet 0)) . toFullStream m
    check (Just a) | a == DigitSet ((2^9)-1) = True
    check _ = False

    step !Nothing _  = Nothing
    step (!Just !acc) (_, !set, _)
      | acc .&. set == DigitSet 0 = Just (acc .|. set)
      | otherwise = trace (show acc ++ show set) Nothing

{-# INLINE allRegions #-}
allRegions :: Monad m => (Range -> m Bool) -> m Bool
allRegions f = do
   a <- shortCutFromToAll 0 8 (f . row)
   if a then return True
   else do
       b <- shortCutFromToAll 0 8 (f . col)
       if b then return True
       else
           shortCutFromToAll 0 2 (\i -> shortCutFromToAll 0 2 (\j -> f (square i j)))


{-# INLINE shortCutFromToAll #-}
shortCutFromToAll :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Bool
shortCutFromToAll zero end p = loop zero
  where
    loop !i
      | i >= end = return True
      | otherwise = do
        r <- p i
        if not r
        then return False
        else loop (i+1)
