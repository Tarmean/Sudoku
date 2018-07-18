{-# Language TypeFamilies #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}
module StreamSlice where
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as B
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util (Id(..))
import Control.Monad.Primitive
import Types
import Shape
import Data.Monoid
import Data.Bits
import GHC.Types ( SPEC(..) )

{-# INLINE anyRegions #-}
anyRegions :: Monad m => (Range -> m Bool) -> m Bool
anyRegions f = do
   a <- shortCutFromTo 0 8 (f . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (f . col)
       if b then return True
       else
           shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> f (square i j)))


{-# INLINE toStream #-}
toStream :: (PrimMonad m, s ~ PrimState m) => Matrix s -> Range -> S.Stream m (Int, DigitSet)
toStream m r = S.map (\(a,b,_) -> (a, b)) . S.filter (\(_,_,b) -> not b) $ toFullStream m r

{-# INLINE toFullStream #-}
toFullStream :: forall s m. (PrimMonad m, s ~ PrimState m) =>  Matrix s -> Range ->S.Stream m (Int, DigitSet, Bool)
toFullStream (Matrix vec) r = S.mapM doRead (liftStream $ rIndices r)
  where
    doRead i = format i <$> G.read vec i
    format i (set, fixed) = (i, set, fixed)
{-# INLINE liftStream #-} 
liftStream :: Monad m => S.Stream Id a -> S.Stream m a
liftStream (S.Stream step s) = S.Stream (return . unId . step) s



-- -- TODO: abstract over Transformation shape and don't pass Matrix
{-# INLINE[1] mapMatrixM #-}
mapMatrixM :: (PrimMonad m) => (Int -> DigitSet -> m Bool) -> Matrix (PrimState m) -> Range -> m Bool
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
      | i >= end = return False
      | otherwise = do
        r <- p i
        if r
        then return True
        else loop (i+1)

{-# INLINE minimumSet #-}
minimumSet :: Monad m => S.Stream m (Int, DigitSet) -> m SMinimum
minimumSet = S.foldl' step SNothing
  where
    step w@(SJust !_ !_ !aCount) (idx, cur )
        | curCount < aCount = SJust idx cur curCount
        | otherwise = w
      where curCount = popCount cur
    step SNothing (!idx, !a) = SJust idx a (popCount a)
