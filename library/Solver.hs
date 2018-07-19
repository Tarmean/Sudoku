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
          -- We unnecessarily recheck unchanged regions a lot
          -- but doing naive card marking tricks is slower still
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
          -- pick the most constrained cell to limit the branching factor
          firstUnfixed <- minimumSet (toStream m allFields)
          case firstUnfixed of
              SJust idx set _ -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)
              SNothing ->
                -- TODO: checking before the entire sudoku is filled for
                -- pruning doesn't seem faster. Why?!
                checkComplete m

-- Probably should return the final matrix as a MaybeMatrix or something
-- but that means mostly duplicating shortCutFromTo.
-- How does the saying go, you don't know how much you rely on laziness until you write a bunch of hacky IO code?
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
