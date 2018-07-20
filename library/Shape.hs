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


fromStepTo :: Int -> Int -> Int -> (Int -> Int -> IO Bool) -> IO Bool
fromStepTo !to !strRedBase !strRedStep cont = loop 0 strRedBase False
  where
    loop !cur !linearIdx !acc
      | cur > to = return acc
      | otherwise = do
          r <- cont linearIdx cur
          loop (cur + 1) (linearIdx + strRedStep) (acc || r)
{-# INLINE fromStepTo #-}
{-# INLINE writeRow #-}
{-# INLINE writeCol #-}
{-# INLINE writeSquare #-}
-- | Build/fold style loop for rows that doesn't shortcut
writeRow :: Int -> (Int -> Int -> Int -> IO Bool) -> IO Bool
writeRow !r cont = fromStepTo 8 (r*9) 1 $ \linIdx i -> cont linIdx r i 
-- | Build/fold style loop for rows that doesn't shortcut
writeCol :: Int -> (Int -> Int -> Int -> IO Bool) -> IO Bool
writeCol !c cont = fromStepTo 8 c 9 $ \linIdx i -> cont linIdx i c

writeSquare :: Int -> Int -> (Int -> Int -> Int -> IO Bool) -> IO Bool
writeSquare !squareRow !squareCol cont
    = fromStepTo 2 (baseRow * 9) 9 $ \strengthReducedRow r ->
      fromStepTo 2 (strengthReducedRow + baseCol) 1 $ \linIdx c -> cont linIdx (baseRow + r) (baseCol + c) 
  where
    baseRow = 3 * squareRow
    baseCol = 3 * squareCol
