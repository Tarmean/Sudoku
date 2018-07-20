{-# Language TypeFamilies #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}
module StreamSlice where
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util (Id(..))
import Types
import Shape
import Data.Bits

{-# INLINE onMatrix #-}
onMatrix :: Matrix -> (Int -> Int -> Int -> DigitSet -> IO Bool) -> (Int -> Int -> Int -> IO Bool)
onMatrix m cont linIdx r c = do
     (set, b) <- readLin m linIdx
     if not b
     then cont linIdx r c set
     else return False

-- Build/fold fusion seems to work a lot better here. 
-- Todo: try build/fold style Range, using strength reduced loops for reading
-- and nested row/col loops for writing
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
toFullStream m r = S.mapM doRead (liftStream $ rIndices r)
  where
    doRead i = format i <$> readLin m i
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
