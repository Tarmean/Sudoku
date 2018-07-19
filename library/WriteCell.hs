{-# Language ScopedTypeVariables #-}
module WriteCell where
import Shape
import Types
import Data.Bits
import StreamSlice

{-# INLINE fixCell #-}
fixCell :: Int -> DigitSet -> Matrix  -> IO ()
fixCell i mask m = do
    writeLin m i (mask, True)
    let
        {-# INLINE apply #-}
        apply = mapMatrixM (applyMask m mask) m

        (r, c) = i `divMod` 9
        sRow = (r `div` 3)
        sCol = (c `div` 3)


    _ <- apply (row r)
    _ <- apply (col c)
    _ <- apply (square sRow sCol)
    return ()

{-# INLINE applyMask #-}
applyMask :: Matrix  -> DigitSet -> Int -> DigitSet -> IO Bool
applyMask m !mask !idx !cur
    |  False = undefined
    | cur' == cur =  return False
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

