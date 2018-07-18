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


solve ::  Matrix  (PrimState IO) -> IO Bool
solve !m = debug '0' m >> loopHiddenSingletons
  where
      loopHiddenSingletons = do
          r <- applyHiddenSingletons m
          debug 's' m
          if r
          then loopHiddenSingletons
          else loopPreemptives
      loopPreemptives = do
          r <- applyPreemptives m
          debug 'p' m
          if r
          then loopHiddenSingletons
          else  recurse
      recurse = do

          firstUnfixed <- minimumSet (toStream m allFields)
          case firstUnfixed of
              SJust idx set _ -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)

              SNothing -> checkComplete m

{-# INLINE doRecursion #-}
doRecursion :: (m ~ IO) => DigitSet -> Int -> Int -> Matrix  (PrimState m) -> m Bool
doRecursion !oldSet !idx !curTry !m
    | not (mask `isSubsetOf` oldSet) = return False
    | otherwise = do
        vec' <-  G.clone (mCells  m)
        let m' = (Matrix vec')
        -- putStrLn ">>>>>>>RECURSION"
        -- changeIndent 1 
        fixCell idx mask m'
        -- debug 'r' m'
        r <- solve m'
        -- changeIndent (-1) 
        -- putStrLn "<<<<<<<RECURSION"
        return r

    where !mask = toDigitSet curTry
