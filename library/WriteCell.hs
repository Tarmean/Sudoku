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
fixCell :: forall m. (PrimMonad m) => Int -> DigitSet -> Matrix (PrimState m) -> m ()
fixCell i mask m = do
    (oldMask, oldFixed) <- G.read (mCells m) i
    writeLin m i (mask, True)
    let
        {-# INLINE apply #-}
        apply = mapMatrixM (applyMask m mask) m

        (r, c) = i `divMod` 9
        sRow = (r `div` 3)
        sCol = (c `div` 3)
    trace (show (r, c) ++ ": old=(" ++ show oldMask ++ "," ++ show oldFixed ++ "), new(" ++ show mask ++ ")") (return ())

    _ <- apply (row r)
    _ <- apply (col c)
    _ <- apply (square sRow sCol)
    return ()

{-# INLINE applyMask #-}
applyMask :: (PrimMonad m) => Matrix (PrimState m) -> DigitSet -> Int -> DigitSet -> m Bool
applyMask m !mask !idx !cur
    | cur `isSubsetOf` mask = return False
    | cur' == cur = return False
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

