{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
module PreemptivePass (applyPreemptives) where
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits
-- import GHC.Types ( SPEC(..) )

import Types
import WriteCell
import StreamSlice

import Data.Vector.Fusion.Util (unId)
import Data.Vector.Fusion.Stream.Monadic (toList)
import Shape
import Trace

{-# INLINE applyPreemptives #-}
applyPreemptives :: (PrimMonad IO) => Matrix  (PrimState IO) -> IO Bool
applyPreemptives = anyRegions . preemptivePass

{-# INLINE preemptivePass #-}
preemptivePass :: (PrimMonad IO) => Matrix (PrimState IO) -> Range -> IO Bool
-- preemptivePass m r | trace ("Preemptive Pass: " ++ replicate 50 '-') False = undefined
preemptivePass m r = searchPreemptives applySet (toStream m r)
  where
    {-# INLINE applySet #-}
    applySet :: DigitSet -> IO Bool
    applySet !mask = do
        let (Range ra _) = r
        trace ("preemptives: " ++ show mask ++ " - " ++ show (map get2D $ unId $ toList ra)) (return ())
        mapMatrixM (applyMask m mask) m r

{-# INLINE searchPreemptives #-}
searchPreemptives :: Monad IO => (DigitSet -> IO Bool) -> S.Stream IO (Int, DigitSet) -> IO Bool
searchPreemptives applySet (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    -- spec const makes this a ton slower
    loop !state !count !set = do
        m <- step state
        case m of
            S.Done -> return False
            S.Skip state' -> loop state' count set
            S.Yield (idx, a) state' -> do
                let set' = a .|. set
                trace (replicate (2*count) ' ' ++ show (count+1) ++ "/" ++ show (popCount set') ++ ", " ++ show (get2D idx) ++ ": " ++ show set') (return ())
                r <- (check state' (count+1) (set'))
                if r then return True
                else loop state' count set
    {-# INLINE check #-}
    check !state' !count' !set'
        | count' == pCount = tindent "FIND!" count' (applySet set')
        | count' >= 4 = tindent "count fail" count' (return False)
        | pCount > 4 = tindent ("pcount fail") count' $ return False
        | otherwise = tindent "loop" count' $ loop state' count' set'
        where pCount = popCount set'
tindent :: String -> Int -> a -> a
tindent s i = trace (replicate (i * 2) ' ' ++ s )
