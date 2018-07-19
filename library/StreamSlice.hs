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
import GHC.Types ( SPEC(..) )

{-# INLINE anyRegions #-}
anyRegions :: (Range -> IO Bool) -> IO Bool
anyRegions f = do
   a <- shortCutFromTo 0 8 (f . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (f . col)
       if b then return True
       else
           shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> f (square i j)))

data RegionState = R1  | R2 | R3
{-# INLINE regions #-}
regions :: S.Stream IO Range
regions = S.Stream step (R1, 0)
  where
    step (!R1, !i)
      | i >= 9 = return $ S.Skip (R2, 0)
      | otherwise = return $ S.Yield (row i) (R1, i+1)
    step (!R2, !i)
      | i >= 9 = return $ S.Skip (R3, 0)
      | otherwise = return $ S.Yield (col i) (R2, i+1)
    step (!R3, !i)
      | i >= 9 = return $ S.Done
      | otherwise = return $ S.Yield (uncurry square $ divMod i 3) (R3, i+1)


{-# INLINE toStream #-}
toStream :: Matrix -> Range -> S.Stream IO (Int, DigitSet)
toStream m r = S.map (\(a,b,_) -> (a, b)) . S.filter (\(_,_,b) -> not b) $ toFullStream m r

{-# INLINE toFullStream #-}
toFullStream :: Matrix -> Range -> S.Stream IO (Int, DigitSet, Bool)
toFullStream (Matrix vec) r = S.mapM doRead (liftStream $ rIndices r)
  where
    doRead i = format i <$> G.read vec i
    format i (set, fixed) = (i, set, fixed)
{-# INLINE liftStream #-} 
liftStream :: S.Stream Id a -> S.Stream IO a
liftStream (S.Stream step s) = S.Stream (return . unId . step) s



-- -- TODO: abstract over Transformation shape and don't pass Matrix
{-# INLINE[1] mapMatrixM #-}
mapMatrixM :: (Int -> DigitSet -> IO Bool) -> Matrix -> Range -> IO Bool
mapMatrixM f m r = S.foldlM (\a (i, set) -> (a ||)  <$> f i set) False (toStream m r)
         

{-# INLINE sFold #-}
sFold :: (Monad m, Monoid acc) => (b -> m acc) -> S.Stream m b -> m acc
sFold f = S.foldlM step mempty
  where
    {-# INLINE step #-}
    step !a !b = fmap (a <>) (f b)

{-# INLINE sShortCircuit #-}
sShortCircuit :: (Monad m) => (r -> b -> m r) -> r -> (r -> Bool) -> S.Stream m b -> m Bool
sShortCircuit f z p (S.Stream step state0) = loop SPEC z state0
  where
    loop !_ !acc !s = do
        m <- step s
        case m of
            S.Skip s' -> loop SPEC acc s'
            S.Done -> return False
            S.Yield a s' -> do
                acc' <- f acc a
                if p acc'
                then return True
                else loop SPEC acc' s'


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

{-# INLINE minimumSet #-}
minimumSet :: Monad m => S.Stream m (Int, DigitSet) -> m SMinimum
minimumSet = S.foldl' step SNothing
  where
    step !w@(SJust !_ !_ !aCount) (!idx, !cur )
        | curCount < aCount = SJust idx cur curCount
        | otherwise = w
      where curCount = popCount cur
    step SNothing (!idx, !a) = SJust idx a (popCount a)
