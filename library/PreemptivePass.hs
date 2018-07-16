{-# Language ScopedTypeVariables #-}
module PreemptivePass (applyPreemptives) where
import Control.Monad.Primitive
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits
import GHC.Types ( SPEC(..) )

import Types
import WriteCell
import StreamSlice

{-# INLINE applyPreemptives #-}
applyPreemptives :: (PrimMonad m) => Matrix  (PrimState m) -> m Bool
applyPreemptives = allRegions . preemptivePass

{-# INLINE preemptivePass #-}
preemptivePass :: forall m. (PrimMonad m) => Matrix (PrimState m) -> Range -> m Bool
preemptivePass m r = searchPreemptives applySet (S.map snd $ toStream m r)
  where
    {-# INLINE applySet #-}
    applySet :: DigitSet -> m Bool
    applySet !mask = mapMatrixM (applyMask m mask) m r

{-# INLINE searchPreemptives #-}
searchPreemptives :: Monad m => (DigitSet -> m Bool) -> S.Stream m DigitSet -> m Bool
searchPreemptives applySet (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    -- spec const makes this a ton slower
    loop !state !count !set = do
        m <- step state
        case m of
            S.Done -> return False
            S.Skip state' -> loop state' count set
            S.Yield a state' -> do
                r <- (check state' (count+1) (a .|. set))
                if r then return True
                else loop state' count set
    {-# INLINE check #-}
    check !state' !count' !set'
        | count' == popCount set' = applySet set'
        | count' >= 4 = return False
        | otherwise = loop state' count' set'
