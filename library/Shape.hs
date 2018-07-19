{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language ConstraintKinds #-}
module Shape where
import Types
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util (unId)

sudokuRows, sudokuCols :: Int
sudokuCols = 9
sudokuRows = 9

{-# INLINE fromToStep #-}
{-# INLINE row #-}
{-# INLINE col #-}
{-# INLINE square #-}
{-# INLINE writeSquare #-}
{-# INLINE allFields #-}
-- this is horribly messy but everything cleaner was ~20% slower
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


-- In fixcell we propagate found cells to the row/column/square
-- writeSquare only does the (non-overlapping) corners.
-- using writeSquare slows down the program by a whopping 20% for some reason, though
writeSquare :: Int -> Int -> Range
writeSquare !r !c = Range (S.Stream step 0)
  where
    offset = 27 * r + 3 * c
    calc i = 
        let (rOffset, cOffset) = (i `quotRem` 2)
        in offset + 18 * rOffset + 2 * cOffset
    step i 
      | i < 4 = pure $ S.Yield (calc i) (i+1)
      | otherwise = pure S.Done

data SPair = SPair !Int !Int
square :: Int -> Int -> Range
square !r !c = Range (S.Stream (squareStep r c) (SPair 0 0))

squareStep :: Applicative f => Int -> Int -> SPair -> f (S.Step SPair Int)
squareStep r c = step
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

squareAround :: Int -> Range
squareAround idx = square rCol cCol
  where
    (r, c) = get2D idx
    rCol = r `div` 3
    cCol = c `div` 3

