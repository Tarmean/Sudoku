{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
module PreemptivePass (applyPreemptives) where
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits
-- import GHC.Types ( SPEC(..) )

import Types
import WriteCell
import StreamSlice


-- {-# INLINE applyPreemptives #-}
applyPreemptives :: Matrix  -> IO Bool
applyPreemptives !m = anyRegions (preemptivePass m)

{-# INLINE preemptivePass #-}
preemptivePass :: Matrix -> Range -> IO Bool
preemptivePass !m !r = searchPreemptives applySet (toStream m r)
  where applySet !mask = mapMatrixM (applyMask m mask) m r

{-# INLINE searchPreemptives #-}
searchPreemptives :: (DigitSet -> IO Bool) -> S.Stream IO (Int, DigitSet) -> IO Bool
searchPreemptives applySet (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    -- spec const makes this a ton slower
    loop !state !count !set = do
        m <- step state
        case m of
            S.Done -> return False
            S.Skip state' -> loop state' count set
            S.Yield (_, a) state' -> do
                let set' = a .|. set
                r <- (check state' (count+1) (set'))
                if r then return True
                else loop state' count set
    {-# INLINE check #-}
    check !state' !count' !set'
        | count' == pCount = (applySet set')
        | count' >= 4 = (return False)
        | pCount > 4 = return False
        | otherwise =  loop state' count' set'
        where pCount = popCount set'
