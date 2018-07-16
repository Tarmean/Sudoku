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
import GHC.Types ( SPEC(..) )

{-# INLINE allRegions #-}
allRegions :: Monad m => (Range -> m Bool) -> m Bool
allRegions f = do
   a <- shortCutFromTo 0 8 (f . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (f . col)
       if b then return True
       else
           shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> f (square i j)))

{-# INLINE streamSegment #-}
streamSegment :: (PrimMonad m, s ~ PrimState m) => Matrix s -> Range -> B.Bundle m (v s) (Int, DigitSet)
streamSegment m r = B.fromStream (toStream m r) (B.Exact (rLen r))

{-# INLINE toStream #-}
toStream :: (PrimMonad m, s ~ PrimState m) => Matrix s -> Range -> S.Stream m (Int, DigitSet)
toStream m r = S.map (\(a,b,_) -> (a, b)) . S.filter (\(_,_,b) -> not b) $ toFullStream m r

{-# INLINE toFullStream #-}
toFullStream :: forall s m. (PrimMonad m, s ~ PrimState m) =>  Matrix s -> Range ->S.Stream m (Int, DigitSet, Bool)
toFullStream (Matrix vec) r = S.mapM doRead (liftStream $ rIndices r)
  where
    doRead i = format i <$> G.read vec i
    format i (set, fixed) = (i, set, fixed)
liftStream :: Monad m => S.Stream Id a -> S.Stream m a
liftStream (S.Stream step s) = S.Stream (return . unId . step) s



-- -- TODO: abstract over Transformation shape and don't pass Matrix
{-# INLINE[1] mapMatrixM #-}
mapMatrixM :: (PrimMonad m) => (Int -> DigitSet -> m Bool) -> Matrix (PrimState m) -> Range -> m Bool
mapMatrixM f m r = getAny <$> bFold (\(i, j) -> Any <$> f i j) (streamSegment m r)
         
{-# INLINE[1] shortcutMatrixM #-}
shortcutMatrixM :: ( PrimMonad m) => (Int -> DigitSet -> m Bool) -> Matrix (PrimState m) -> Range -> m Bool
shortcutMatrixM f m r = sShortCircuit (uncurry f) (toStream m r)

{-# INLINE[1] bFold #-}
bFold :: (Monad m, Monoid acc) => (b -> m acc) -> B.Bundle m v b -> m acc
bFold f = B.foldlM step mempty
  where
    {-# INLINE step #-}
    step !a !b = fmap (a <>) (f b)

{-# INLINE[1] sShortCircuit #-}
sShortCircuit :: (Monad m) => (b -> m Bool) ->  S.Stream m b -> m Bool
sShortCircuit f (S.Stream step state0) = loop SPEC state0
  where
    loop !_ s = do
        m <- step s
        case m of
            S.Skip s' -> loop SPEC s'
            S.Done -> return False
            S.Yield a s' -> do
                r <- f a
                if r
                then return True
                else loop SPEC s'


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

headMaybe :: Monad m => S.Stream m a -> m (SMaybe a)
{-# INLINE[1] headMaybe #-}
headMaybe (S.Stream step t) = head_loop SPEC t
  where
    head_loop !_ s
      = do
          r <- step s
          case r of
            S.Yield x _  -> return (SJust x)
            S.Skip    s' -> head_loop SPEC s'
            S.Done       -> return SNothing
