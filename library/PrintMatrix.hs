{-# Language TypeFamilies #-}
module PrintMatrix (printMatrix) where
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Control.Monad.Primitive

import Types
import StreamSlice
import Shape

import Control.Monad

printMatrix :: Matrix  RealWorld -> IO ()
printMatrix m =
     forM_ [0..8] $ \i -> do
         rowString <- formatRow $ getRow i
         putStrLn rowString
  where
    getRow i = S.map (\(_,d,b) -> (d, b)) $ toFullStream m (row i)
    formatRow s = S.foldl (\acc (c, b) -> acc . mark b . shows c) id s <*> pure ""
    mark b = if b then ('|' :) else ('.' :)
