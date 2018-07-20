{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language MultiWayIf #-}
module PreemptivePass (applyPreemptives) where
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Bits
-- import GHC.Types ( SPEC(..) )

import Types
import WriteCell
import StreamSlice
import Shape


-- {-# INLINE applyPreemptives #-}
applyPreemptives :: Matrix  -> IO Bool
applyPreemptives !m = do
   a <- shortCutFromTo 0 8 (preemptivePass m . row)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (preemptivePass m . col)
       if b then return True
       else shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> preemptivePass m (square i j)))

{-# INLINE preemptivePass #-}
preemptivePass :: Matrix -> Range -> IO Bool
preemptivePass !m !r = searchPreemptives applySet (toStream m r)
  where applySet !mask = mapMatrixM (applyMask m mask) m r

{-# INLINE searchPreemptives #-}
searchPreemptives :: (DigitSet -> IO Bool) -> S.Stream IO (Int, DigitSet) -> IO Bool
searchPreemptives applySet (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    -- TODO: figure out why SPEC'ing this destry performance
    loop state !count !set = do
        m <- step state
        case m of
            S.Done -> return False
            S.Skip state' -> loop state' count set
            S.Yield (_, a) state' -> do
                let set' = a .|. set
                    count' = count + 1
                    pCount = popCount set'
                    jmp = loop state' count set
                if 
                  | count' == pCount  -> applySet set'
                  | count' >= 4 -> jmp
                  | pCount > 4 -> jmp
                  | otherwise -> do
                      r <- loop state' count' set'
                      if r then return True else jmp
