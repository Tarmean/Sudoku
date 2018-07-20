{-# Language TypeFamilies #-}
module PrintMatrix  where
import qualified Data.Vector.Fusion.Stream.Monadic as S

import Types
import StreamSlice
import Shape

import Control.Monad
import Text.Printf
import System.IO.Unsafe

instance PrintfArg Matrix where
   formatArg m _ = (unlines (unsafePerformIO (formatMatrix m)) ++)


printMatrix :: Matrix -> IO ()
printMatrix m = mapM_ putStrLn  =<< formatMatrix m
formatMatrix :: Matrix  -> IO [String]
formatMatrix m =
     forM [0..8] $ \i -> do
         formatRow $ getRow i
  where
    getRow i = S.map (\(_,d, b) -> (d, b)) $ toFullStream m (row i)
    formatRow !s = S.foldl (\acc (c, b) -> acc . mark b . shows c) id s <*> pure ""
    mark b = if b then ('|' :) else ('.' :)
