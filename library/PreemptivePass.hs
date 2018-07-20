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
   a <- shortCutFromTo 0 8 $ \r -> do
       out <- searchPreemptives (toStream m $ row r)
       case out of
         DNothing -> pure False
         DJust set -> writeRow r $ onMatrix m (applyMask m set)
   if a then return True else do
       b <- shortCutFromTo 0 8 $ \c -> do
           out <- searchPreemptives (toStream m $ col c)
           case out of
             DNothing -> pure False
             DJust set -> writeCol c $ onMatrix m (applyMask m set)
       if b then return True else do
           shortCutFromTo 0 2 $ \r -> shortCutFromTo 0 2 $ \c -> do
               out <- searchPreemptives (toStream m $ square r c)
               case out of
                 DNothing -> pure False
                 DJust set -> writeSquare r c $ onMatrix m (applyMask m set)
               

{-# INLINE searchPreemptives #-}
searchPreemptives :: S.Stream IO (Int, DigitSet) -> IO MDigitSet
searchPreemptives (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    -- TODO: SPEC'ing destroys performance. Is it because loop isn't a join
    -- point?  Using a strict list as stack is only ~11% slower so using an
    -- unboxed array might be faster
    loop !state !count !set = do
        m <- step state
        case m of
            S.Done -> return DNothing
            S.Skip state' -> loop state' count set
            S.Yield (_, a) state' -> do
                let set' = a .|. set
                    count' = count + 1
                    pCount = popCount set'
                    jmp = loop state' count set
                if 
                  | count' == pCount  -> return (DJust set')
                  | count' >= 4 -> jmp
                  | pCount > 4 -> jmp
                  | otherwise -> do
                      r <- loop state' count' set'
                      case r of
                        DNothing -> jmp
                        _ -> return r
