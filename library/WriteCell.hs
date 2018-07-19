{-# Language ScopedTypeVariables #-}
module WriteCell where
import Shape
import Types
import Data.Bits
import StreamSlice


-- This propagates found numbers, potentially recursively
-- that means we mutate the matrix while streaming parts of it but that's fine since
-- a `isSubsetOf` b => forall c. (a \\ c) `isSubsetOf` b
-- it might have some weird performance problem so TODO: profile an alternative where
-- we loop over the matrix until we find a fixpoint after each change
{-# INLINE fixCell #-}
fixCell :: Int -> DigitSet -> Matrix  -> IO ()
fixCell !i !mask !m = do
    writeLin m i (mask, True)
    let
        (r, c) = i `quotRem` 9
        sRow = (r `quot` 3)
        sCol = (c `quot` 3)


    _ <- mapMatrixM (applyMask m mask) m (row r)
    _ <- mapMatrixM (applyMask m mask) m (col c)
    _ <- mapMatrixM (applyMask m mask) m (square sRow sCol)
    return ()

-- for ~~REASONS~~ this slows way down when m is strict
{-# INLINE applyMask #-}
applyMask :: Matrix  -> DigitSet -> Int -> DigitSet -> IO Bool
applyMask m !mask !idx !cur
    -- This is important, otherwise PreemptivePass thinks it's helping when all
    -- other entries don't overlap with the preemptive set and loops forever
    | cur' == cur =  return False
    -- This is a hack so we don't have to keep track which entries made up a PreemptivePass pair
    | cur `isSubsetOf` mask = return False
    | isSingleton cur' = do
        fixCell idx cur' m
        return True
    | otherwise = do
        writeLin m idx (cur', False) 
        return  True
    where cur' = cur \\ mask

{-# INLINE (\\) #-}
{-# INLINE isSubsetOf #-}
isSubsetOf :: DigitSet -> DigitSet -> Bool
isSubsetOf a b = (b .|. a) == b
(\\) :: DigitSet -> DigitSet -> DigitSet
(\\) a b = complement b .&. a

{-# INLINE isSingleton #-}
isSingleton :: DigitSet -> Bool
isSingleton s = popCount s == 1
