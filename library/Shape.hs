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
{-# INLINE writeSquare #-}
{-# INLINE allFields #-}
fromToStep :: Int -> Int -> Int -> Int -> Range
fromToStep from to step size = Range (S.Stream f from) size
  where
    f !i
      | i < to = pure $ S.Yield i (i + step)
      | otherwise = pure S.Done

allFields :: Range
allFields = fromToStep 0 81 1 81
row :: Int -> Range
row r = fromToStep from to step 9
  where
    from = 9 * r
    to = from + 9 * step
    step = 1

col :: Int -> Range
col c = fromToStep from to step 9
  where
    from = c
    to = sudokuCols * 9 + c
    step = sudokuCols

data Corner = UL | UR | LL | LR | Done

-- In fixcell we propagate found cells to the row/column/square
-- writeSquare only does the (non-overlapping) corners.
-- using writeSquare slows down the program by a whopping 20% for some reason, though
writeSquare :: Int -> Int -> Range
writeSquare !r !c = Range (S.Stream step UL) 3
  where
    offset = 27 * r + 3 * c
    calc a b = offset + 9 * a + b
    checked !a !b !n = 
     let idx = calc a b
     in if idx < 81 then pure (S.Yield idx n) else pure (S.Skip n)
    step UL = checked 0 0 UR
    step UR = checked 0 3 LL
    step LL = checked 3 0 LR
    step LR = checked 3 3 Done
    step Done = pure S.Done

data SPair = SPair !Int !Int
square :: Int -> Int -> Range
square !r !c = Range (S.Stream step (SPair 0 0)) 9
  where
    offset = r * 27 + 3 * c
    calc !a !b = offset + a + b * 9
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

get2D :: Int -> (Int, Int)
get2D i = (row, col)
  where (row, col) = i `divMod` 9
