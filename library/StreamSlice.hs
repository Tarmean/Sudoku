{-# Language TypeFamilies #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}
module StreamSlice where
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util (Id(..))
import Types
import Shape
import Data.Bits

-- This weird control ping pong is immensely ugly. This is basically
-- `shortcutPass pass ranges = S.or (S.map pass ranges)` but we can't abstract over
-- the if blocks because `shortcutPass pass (rows S.++ cols S.++ squares)`
-- is ~50-100% slower
{-# INLINE anyRegions #-}
anyRegions :: (Range -> IO Bool) -> IO Bool
anyRegions f = do
   a <- shortCutFromTo 0 8 (f . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (f . col)
       if b then return True
       else shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> f (square i j)))

{-# INLINE toStream #-}
toStream :: Matrix -> Range -> S.Stream IO (Int, DigitSet)
toStream m r = S.map (\(idx,val,_) -> (idx, val)) . S.filter (\(_,_,b) -> not b) $ toFullStream m r

{-# INLINE toFullStream #-}
toFullStream :: Matrix -> Range -> S.Stream IO (Int, DigitSet, Bool)
toFullStream (Matrix vec) r = S.mapM doRead (liftStream $ rIndices r)
  where
    doRead i = format i <$> G.read vec i
    format i (set, fixed) = (i, set, fixed)
{-# INLINE liftStream #-} 
liftStream :: S.Stream Id a -> S.Stream IO a
liftStream (S.Stream step s) = S.Stream (return . unId . step) s

{-# INLINE mapMatrixM #-}
mapMatrixM :: (Int -> DigitSet -> IO Bool) -> Matrix -> Range -> IO Bool
mapMatrixM f m r = S.foldlM (\a (i, set) -> (a ||)  <$> f i set) False (toStream m r)

{-# INLINE shortCutFromTo #-}
shortCutFromTo :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Bool
shortCutFromTo zero end p = loop zero
  where
    loop i
      | i > end = return False
      | otherwise = do
        r <- p i
        if r
        then return True
        else loop (i+1)

-- Doing this seperately from TestSolution.checkComplete is still faster
-- because the fusion would compare each element 3 times
{-# INLINE minimumSet #-}
minimumSet :: Monad m => S.Stream m (Int, DigitSet) -> m SMinimum
minimumSet = S.foldl' step SNothing
  where
    step !w@(SJust !_ !_ !aCount) (!idx, !cur )
        | curCount < aCount = SJust idx cur curCount
        | otherwise = w
      where curCount = popCount cur
    step SNothing (!idx, !a) = SJust idx a (popCount a)
