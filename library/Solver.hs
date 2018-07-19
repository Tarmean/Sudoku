{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
module Solver (solve) where
import qualified Data.Vector.Generic.Mutable as G
import PreemptivePass
import HiddenSingletonPass
import Types
import Shape
import WriteCell
import StreamSlice
import TestSolution


solve ::  Matrix  -> IO Bool
solve !m = loopHiddenSingletons
  where
      loopHiddenSingletons = do
          r <- applyHiddenSingletons m
          if r
          then loopHiddenSingletons
          else loopPreemptives
      loopPreemptives = do
          r <- applyPreemptives m
          if r
          then loopHiddenSingletons
          else  recurse
      recurse = do

          firstUnfixed <- minimumSet (toStream m allFields)
          case firstUnfixed of
              SJust idx set _ -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)

              SNothing -> checkComplete m

{-# INLINE doRecursion #-}
doRecursion :: DigitSet -> Int -> Int -> Matrix   -> IO Bool
doRecursion !oldSet !idx !curTry !m
    | not (mask `isSubsetOf` oldSet) = return False
    | otherwise = do
        vec' <-  G.clone (mCells  m)
        let m' = (Matrix vec')
        fixCell idx mask m'
        solve m'

    where !mask = toDigitSet curTry
