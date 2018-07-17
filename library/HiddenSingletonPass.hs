{-# Language ScopedTypeVariables #-}
module HiddenSingletonPass (applyHiddenSingletons) where
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits

import Types
import WriteCell
import StreamSlice
import Trace

-- {-# INLINE applyHiddenSingletons #-}
applyHiddenSingletons ::  Matrix (PrimState IO) -> IO Bool
applyHiddenSingletons !m = anyRegions (hiddenSingletonPass m)

{-# INLINE hiddenSingletonPass #-}
hiddenSingletonPass :: forall m. (PrimMonad m) => Matrix (PrimState m) -> Range -> m Bool
hiddenSingletonPass !m !r =  do
    SPair covered overlap <- S.foldl' step (SPair notFound notFound) (toStream m r)
    let mask = covered \\ overlap
        {-# INLINE fixFinds #-}
        fixFinds !idx !set = case set .&. mask of
            found ->
              if found /= notFound
              then const True <$> fixCell idx found m
              else return False
    if mask /= notFound
    then mapMatrixM fixFinds m r
    else return False
  where
    {-# INLINE step #-}
    step (SPair covered overlap) (_,i) = SPair (covered .|. i) (overlap .|. (covered .&. i))
    notFound = DigitSet 0

data SPair = SPair !DigitSet !DigitSet
