module TestSolution where


import Data.Bits
import Types
import Shape
import StreamSlice
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Trace


data Result = Success | NotDone | Failure
{-# INLINE checkComplete #-}
checkComplete :: PrimMonad m => Matrix (PrimState m) -> m Result
checkComplete m = allRegions pass
  where
    pass = fmap check . S.foldl' step (Just (DigitSet 0)) . toFullStream m
    check (Just a)
        | a == allSet = Success
        | otherwise = NotDone
    check Nothing = Failure
    allSet = DigitSet ((2^(9::Int))-1)

    step Nothing _  = Nothing
    step (Just acc) (_, set, _)
      | acc .&. set == DigitSet 0 = Just (acc .|. set)
      | otherwise = trace ("Check failure :" ++ show acc ++ show set) Nothing

{-# INLINE allRegions #-}
allRegions :: Monad m => (Range -> m Bool) -> m Bool
allRegions f = do
   a <- shortCutFromToAll 0 8 (f . row)
   if a /= Success then return a
   else do
       b <- shortCutFromToAll 0 8 (f . col)
       if b /= Success then return b
       else
           shortCutFromToAll 0 2 (\i -> shortCutFromToAll 0 2 (\j -> f (square i j)))


{-# INLINE shortCutFromToAll #-}
shortCutFromToAll :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Bool
shortCutFromToAll zero end p = loop zero
  where
    loop i
      | i >= end = return True
      | otherwise = do
        r <- p i
        if not r
        then return False
        else loop (i+1)
