{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE  FlexibleContexts#-}
module Types where
import Data.Vector.Unboxed 
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import Control.Monad (liftM)
import Data.Primitive.Types
import Data.Bits
import GHC.Word
import qualified Data.Vector.Unboxed.Mutable as U

data Matrix d s
    = Matrix
    { mLayout :: !d
    , mCells ::  U.MVector s (DigitSet, Bool)
    }

newtype DigitSet = DigitSet Word16
  deriving (Bits, Eq, Prim)

toDigitSet :: Int -> DigitSet
toDigitSet i = DigitSet 1 `shiftL` i
instance Unbox DigitSet
setFromList :: [Int] -> DigitSet
setFromList ls = DigitSet (loop [1..9] ls 0)
  where
    loop (x:xs) (y:ys) i
       | x == y = loop xs ys ((i .|. nine) `shiftR` 1)
   
    loop (_:xs) r i  = loop xs r (i `shiftR` 1)
    loop [] _ i = i

    nine = 1 `shiftL` 9
instance Show DigitSet where
    showsPrec _ (DigitSet i0) = ('[':) . loop ['1'..'9'] i0 . (']':)
      where
        loop [] _ = id
        loop (x:xs) i
          | (i .&. 1) > 0 = (x:) . loop xs (i `shiftR` 1)
          | otherwise =  ('_':) . loop xs (i `shiftR` 1)

newtype instance MVector s DigitSet = MV_DigitSet (P.MVector s DigitSet)
newtype instance Vector    DigitSet = V_DigitSet  (P.Vector    DigitSet)

-- deriving doesn't work because ghc assumes MVector's parameters are nominal
instance M.MVector MVector DigitSet where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_DigitSet v) = M.basicLength v
    basicUnsafeSlice i n (MV_DigitSet v) = MV_DigitSet $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_DigitSet v1) (MV_DigitSet v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_DigitSet `liftM` M.basicUnsafeNew n
    basicInitialize (MV_DigitSet v) = M.basicInitialize v
    basicUnsafeReplicate n x = MV_DigitSet `liftM` M.basicUnsafeReplicate n x
    basicUnsafeRead (MV_DigitSet v) i = M.basicUnsafeRead v i
    basicUnsafeWrite (MV_DigitSet v) i x = M.basicUnsafeWrite v i x
    basicClear (MV_DigitSet v) = M.basicClear v
    basicSet (MV_DigitSet v) x = M.basicSet v x
    basicUnsafeCopy (MV_DigitSet v1) (MV_DigitSet v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_DigitSet v1) (MV_DigitSet v2) = M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_DigitSet v) n = MV_DigitSet `liftM` M.basicUnsafeGrow v n
instance G.Vector Vector DigitSet where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_DigitSet v) = V_DigitSet `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_DigitSet v) = MV_DigitSet `liftM` G.basicUnsafeThaw v
    basicLength (V_DigitSet v) = G.basicLength v
    basicUnsafeSlice i n (V_DigitSet v) = V_DigitSet $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_DigitSet v) i = G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_DigitSet mv) (V_DigitSet v) = G.basicUnsafeCopy mv v
    elemseq _ = seq

