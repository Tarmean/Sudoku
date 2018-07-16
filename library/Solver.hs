{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
module Solver (solve) where
import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as G
import PreemptivePass
import HiddenSingletonPass
import Types
import Shape
import WriteCell
import StreamSlice
import PrintMatrix
import TestSolution

{-# INLINE solve #-}
solve ::  Matrix  (PrimState IO) -> IO Bool
solve m = debug 's' >> loopHiddenSingletons
  where
      loopHiddenSingletons = do
          r <- applyHiddenSingletons m
          debug 'h'
          if r
          then loopHiddenSingletons
          else loopPreemptives
      loopPreemptives = do
          r <- applyPreemptives m
          debug 'p'
          if r
          then loopHiddenSingletons
          else  recurse
      recurse = do
          firstUnfixed <- minimumSet (toStream m allFields)
          debug 'r'
          case firstUnfixed of
              SJust idx set _ -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)
              SNothing -> checkComplete m
      debug x = return ()
        -- = do
        --   putStrLn (x:replicate 107 '-')
        --   printMatrix m
        --
{-# INLINE doRecursion #-}
doRecursion :: (m ~ IO) => DigitSet -> Int -> Int -> Matrix  (PrimState m) -> m Bool
doRecursion oldSet idx curTry m
    | not (mask `isSubsetOf` oldSet) = return False
    | otherwise = do
        vec' <-  G.clone (mCells  m)
        let m' = (Matrix vec')
        fixCell idx mask m'
        solve m'
    where mask = toDigitSet curTry
