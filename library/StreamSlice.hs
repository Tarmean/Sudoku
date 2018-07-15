{-# Language TypeFamilies #-}
{-# Language ExistentialQuantification #-}
module StreamSlice where
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import qualified Data.Vector.Fusion.Bundle.Size as B
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Control.Monad.Primitive
import Types
import Shape

{-# INLINE streamSegment #-}
streamSegment :: (LayoutI layout, PrimMonad m, s ~ PrimState m) => Matrix layout s -> B.Bundle m (v s) (Int, DigitSet)
streamSegment m = B.fromStream (toStream m) (B.Exact len)
  where len = size $ extend $ mLayout m

{-# INLINE toStream #-}
toStream :: (LayoutI layout, PrimMonad m, s ~ PrimState m) => Matrix layout s -> S.Stream m (Int, DigitSet)
toStream = S.map (\(a,b,_) -> (a, b)) . S.filter (\(_,_,b) -> not b) . toFullStream

{-# INLINE toFullStream #-}
toFullStream :: (LayoutI layout, PrimMonad m, s ~ PrimState m) => Matrix layout s -> S.Stream m (Int, DigitSet, Bool)
toFullStream (Matrix layout vec) = (S.Stream step 0)
  where
    
    {-# INLINE step #-}
    step !i 
      | i < len = do
          let idx = toFlatIndex layout i
          (val, isFixed) <- vec `G.read` idx
          pure $ S.Yield (idx, val, isFixed) (i+1)
      | otherwise = pure S.Done
    len = size (extend layout)

{-# INLINE mapMatrix #-}
mapMatrix :: (LayoutI l, PrimMonad m) => (DigitSet -> DigitSet) -> Matrix l (PrimState m) -> m ()
mapMatrix f m@(Matrix _ vec) = mapMatrixM f' m
  where f' idx v = let v' = f v in G.write vec idx (v', False)


-- -- TODO: abstract over Transformation shape and don't pass Matrix
{-# INLINE mapMatrixM #-}
mapMatrixM :: (LayoutI l, PrimMonad m, Monoid acc) => (Int -> DigitSet -> m acc) -> Matrix l (PrimState m) -> m acc
mapMatrixM f m = bFold (uncurry f) (streamSegment m)
         
{-# INLINE shortcutMatrixM #-}
shortcutMatrixM :: (LayoutI l, PrimMonad m) => (Int -> DigitSet -> m Bool) -> Matrix l (PrimState m) -> m Bool
shortcutMatrixM f m = sShortCircuit (uncurry f) (toStream m)

{-# INLINE bFold #-}
bFold :: (Monad m, Monoid acc) => (b -> m acc) -> B.Bundle m v b -> m acc
bFold f = B.foldlM step mempty
  where
    {-# INLINE step #-}
    step !a !b = fmap (a <>) (f b)

{-# INLINE sShortCircuit #-}
sShortCircuit :: (Monad m) => (b -> m Bool) ->  S.Stream m b -> m Bool
sShortCircuit f (S.Stream step state0) = loop state0
  where
    loop s = do
        m <- step s
        case m of
            S.Skip s' -> loop s'
            S.Done -> return False
            S.Yield a s' -> do
                r <- f a
                if r
                then return True
                else loop s'



