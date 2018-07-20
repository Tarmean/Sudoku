{-# Language FlexibleContexts #-}
module HiddenSingletonPass (applyHiddenSingletons) where
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits

import Shape
import Types
import WriteCell
import StreamSlice

-- {-# INLINE applyHiddenSingletons #-}
applyHiddenSingletons ::  Matrix -> IO Bool
applyHiddenSingletons !m = do
   a <- shortCutFromTo 0 8 (hiddenSingletonPass m . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (hiddenSingletonPass m . col)
       if b then return True
       else shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> hiddenSingletonPass m (square i j)))

{-# INLINE hiddenSingletonPass #-}
hiddenSingletonPass :: Matrix -> Range -> IO Bool
hiddenSingletonPass !m !r =  do
    let stream = S.map snd $ toStream m r
    mask <- getSingletons stream
    let {-# INLINE fixFinds #-}
        fixFinds !idx !set = case set .&. mask of
            found ->
              if found /= notFound
              then const True <$> fixCell idx found m
              else return False
    if mask /= notFound
    then do
        mapMatrixM fixFinds m r
    else do
        return False
  where notFound = DigitSet 0
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
