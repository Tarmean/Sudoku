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
    a <- shortCutFromTo 0 8 $ \r -> do
       out <- hiddenSingletonPass m (row r)
       case out of
         DNothing -> pure False
         DJust mask -> writeRow r (onMatrix m (apply mask))
    if a then return True else do
        b <- shortCutFromTo 0 8 $ \c -> do
           out <- hiddenSingletonPass m (col c)
           case out of
             DNothing -> pure False
             DJust mask -> writeCol c (onMatrix m (apply mask))
        if b then return True else do
            shortCutFromTo 0 2 $ \r -> shortCutFromTo 0 2 $ \c -> do
               out <- hiddenSingletonPass m (square r c)
               case out of
                 DNothing -> pure False
                 DJust mask -> writeSquare r c (onMatrix m (apply mask))
  where
    apply mask idx r c val
      | mask .&. val /= notFound =  const True <$> fixCell idx r c (mask .&. val) m
      | otherwise = return False

{-# INLINE hiddenSingletonPass #-}
hiddenSingletonPass :: Matrix -> Range -> IO MDigitSet
hiddenSingletonPass !m !r =  do
    mask <- getSingletons $ S.map snd $ toStream m r
    if mask /= notFound
    then do return (DJust  mask)
    else do return DNothing
notFound :: DigitSet
notFound = DigitSet 0

getSingletons :: S.Stream IO (DigitSet) -> IO DigitSet
getSingletons s = do
    SPair a b <- S.foldl' step (SPair notFound notFound) s
    return (a \\ b)
  where
    {-# INLINE step #-}
    step (SPair covered overlap) i = SPair (covered .|. i) (overlap .|. (covered .&. i))

data SPair = SPair !DigitSet !DigitSet
  deriving Show
