{-# Language ScopedTypeVariables #-}
module WriteCell where
import Control.Monad.Primitive
import Shape
import Types
import Data.Bits
import StreamSlice
import Trace
import Data.Vector.Generic.Mutable as G

{-# INLINE fixCell #-}
fixCell :: Int -> DigitSet -> Matrix (PrimState IO) -> IO ()
fixCell i mask m = do
    writeLin m i (mask, True)
    let
        {-# INLINE apply #-}
        apply = mapMatrixM (applyMask m mask) m

        (r, c) = i `divMod` 9
        sRow = (r `div` 3)
        sCol = (c `div` 3)
    trace ("Fixed : " ++ show (r,c) ++ " to " ++ show mask) (return ())


    changeIndent 1 
    _ <- apply (row r)
    _ <- apply (col c)
    _ <- apply (square sRow sCol)
    changeIndent (-1) 

{-# INLINE applyMask #-}
applyMask :: Matrix (PrimState IO) -> DigitSet -> Int -> DigitSet -> IO Bool
applyMask m !mask !idx !cur
    |  False = undefined
    | cur' == cur = tr "=" >> return False
    | cur `isSubsetOf` mask = tr "SUB" >> return False
    | isSingleton cur' = do
        tr "SING"
        fixCell idx cur' m
        return True
    | otherwise = do
        tr "N"
        writeLin m idx (cur', False) 
        return  True
    where
        cur' = cur \\ mask
        (r, c) = idx `divMod` 9
        tr s = traceIO (show (r, c) ++ s ++" mask=" ++ show mask ++ ": " ++ show cur ++ "=>" ++ show cur')

{-# INLINE (\\) #-}
{-# INLINE isSubsetOf #-}
isSubsetOf :: DigitSet -> DigitSet -> Bool
isSubsetOf a b = (b .|. a) == b
(\\) :: DigitSet -> DigitSet -> DigitSet
(\\) a b = complement b .&. a

{-# INLINE isSingleton #-}
isSingleton :: DigitSet -> Bool
isSingleton s = popCount s == 1

