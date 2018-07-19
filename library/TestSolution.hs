module TestSolution where


import Data.Bits
import Types
import Shape
import StreamSlice
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Trace


{-# INLINE checkComplete #-}
checkComplete :: Matrix -> IO Bool
checkComplete m = allRegions pass
  where
    pass = fmap check . S.foldl' step (Just (DigitSet 0)) . toFullStream m
    check (Just a) | a == allSet = True
    check _ = False
    allSet = DigitSet ((2^(9::Int))-1)

    step Nothing _  = Nothing
    step (Just acc) (_, set, _)
      | acc .&. set == DigitSet 0 = Just (acc .|. set)
      | otherwise = trace ("Check failure :" ++ show acc ++ show set) Nothing

{-# INLINE allRegions #-}
allRegions :: (Range -> IO Bool) -> IO Bool
allRegions f = do
   a <- shortCutFromToAll 0 8 (f . row)
   if not a then return False
   else do
       b <- shortCutFromToAll 0 8 (f . col)
       if not b then return False
       else
           shortCutFromToAll 0 2 (\i -> shortCutFromToAll 0 2 (\j -> f (square i j)))


{-# INLINE shortCutFromToAll #-}
shortCutFromToAll :: Int -> Int -> (Int -> IO Bool) -> IO Bool
shortCutFromToAll zero end p = loop zero
  where
    loop i
      | i >= end = return True
      | otherwise = do
        r <- p i
        if not r
        then return False
        else loop (i+1)
