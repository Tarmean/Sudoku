{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language ConstraintKinds #-}
module Shape where
import Types
import qualified Data.Vector.Fusion.Stream.Monadic as S

sudokuRows, sudokuCols :: Int
sudokuCols = 9
sudokuRows = 9

{-# INLINE fromToStep #-}
{-# INLINE row #-}
{-# INLINE col #-}
{-# INLINE square #-}
{-# INLINE allFields #-}

-- this is horribly messy but everything cleaner was ~20% slower - including S.enumFromStepN
fromToStep :: Int -> Int ->  Int -> Range
fromToStep from to step = Range (S.Stream f from)
  where
    f !i
      | i < to = pure $ S.Yield i (i + step)
      | otherwise = pure S.Done

allFields :: Range
allFields = fromToStep 0 81 1
row :: Int -> Range
row r = fromToStep from to step
  where
    from = 9 * r
    to = from + 9 * step
    step = 1

col :: Int -> Range
col c = fromToStep from to step
  where
    from = c
    to = sudokuCols * 9 + c
    step = sudokuCols


data SLoop = SOuter !Int | SInner !Int !Int
square :: Int -> Int -> Range
square !r !c = Range (S.Stream (squareStep r c) (SOuter (-9)))

-- {-# INLINE squareStep #-}
squareStep :: Applicative f => Int -> Int -> SLoop -> f (S.Step SLoop Int)
squareStep !r !c = step
  where
    offset = r * 27 + 3 * c + 10
    step (SOuter !a)
      | a > 9 = pure S.Done
      | otherwise = pure (S.Skip (SInner a (-1)))
    step (SInner !a !b)
      | b > 1 = pure (S.Skip (SOuter (a+9)))
      | otherwise = pure (S.Yield (offset + a + b) (SInner a (b+1)))
