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
import TestSolution
import Trace

{-# INLINE solve #-}
solve ::  Matrix  (PrimState IO) -> IO Bool
solve m = debug 's' m >> recurse
  where
      -- loopHiddenSingletons = do
      --     r <- applyHiddenSingletons m
      --     debug 'h' m
      --     if r
      --     then loopHiddenSingletons
      --     else recurse
      -- loopPreemptives = do
      --     r <- applyPreemptives m
      --     debug 'p' m
      --     if r
      --     then loopPreemptives
      --     else  recurse
      recurse = do
          firstUnfixed <- minimumSet (toStream m allFields)
          debug 'r' m
          print firstUnfixed
          case firstUnfixed of
              SJust idx set _ -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)
              SNothing -> checkComplete m

{-# INLINE doRecursion #-}
doRecursion :: (m ~ IO) => DigitSet -> Int -> Int -> Matrix  (PrimState m) -> m Bool
doRecursion oldSet idx curTry m
    | not (mask `isSubsetOf` oldSet) = return False
    | otherwise = do
        vec' <-  G.clone (mCells  m)
        let m' = (Matrix vec')
        putStrLn ">>>>>>>RECURSION"
        changeIndent 1 
        fixCell idx mask m'
        r <- solve m'
        changeIndent (-1) 
        putStrLn "<<<<<<<RECURSION"
        return r

    where mask = toDigitSet curTry
