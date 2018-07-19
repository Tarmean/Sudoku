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


data SPair = SPair !Int !Int
square :: Int -> Int -> Range
square !r !c = Range (S.Stream (squareStep r c) (SPair 0 0))

squareStep :: Applicative f => Int -> Int -> SPair -> f (S.Step SPair Int)
squareStep r c = step
  where
    offset = r * 27 + 3 * c
    calc !a !b = offset + a + b * 9
    -- This is really messy. At least we only have one yield
    step !(SPair !a !b) =
       if a < 3
       then
         let idx = calc a b
         in if idx >= 81
            then pure (S.Skip (SPair (a+1) b))
            else pure $ S.Yield idx (SPair (a+1) b)
       else
         if b < 2
         then pure $ S.Skip (SPair 0 (b+1))
         else pure S.Done -- on 3,2 we are done

