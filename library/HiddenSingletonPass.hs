{-# Language FlexibleContexts #-}
module HiddenSingletonPass (applyHiddenSingletons) where
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits

import Types
import WriteCell
import StreamSlice
import Trace
import Shape (gets)

-- {-# INLINE applyHiddenSingletons #-}
applyHiddenSingletons ::  Matrix (PrimState IO) -> IO Bool
applyHiddenSingletons !m = anyRegions (hiddenSingletonPass m)

{-# INLINE hiddenSingletonPass #-}
hiddenSingletonPass :: Matrix (PrimState IO) -> Range -> IO Bool
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
    then do
        traceIO ("hiddenSingleton : " ++ gets r ++ " FOUND : " ++ show mask)
        mapMatrixM fixFinds m r
    else do
        traceIO ("hiddenSingleton : " ++ gets r ++ "NOT FOUND")
        return False
  where
    {-# INLINE step #-}
    step (SPair covered overlap) (_,i) = SPair (covered .|. i) (overlap .|. (covered .&. i))
    notFound = DigitSet 0
getSingletons :: S.Stream IO (DigitSet) -> IO DigitSet
getSingletons s = do
    SPair a b <- S.foldl' step (SPair notFound notFound) s
    return (a \\ b)
  where
    {-# INLINE step #-}
    step (SPair covered overlap) (i) = SPair (covered .|. i) (overlap .|. (covered .&. i))
    notFound = DigitSet 0

data SPair = SPair !DigitSet !DigitSet
  deriving Show
test = map setFromList [[4,6,7,8,9], [3], [1,5,7,8,9], [1,4,5,7,9], [1,2,4,5,7,8], [1,2,4,5,7,8,9], [1,2,5,7,8,9], [1,5,7,8,9], [2,5,7,8,9]]
