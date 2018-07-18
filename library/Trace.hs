module Trace (trace, debug, changeIndent, indentation, traceIO) where
import PrintMatrix
import Control.Monad.Primitive
import Types
-- import Debug.Trace
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE indentation #-}
indentation :: IORef Int
indentation = unsafePerformIO (newIORef 0)

{-# NOINLINE changeIndent #-}
changeIndent :: Int -> IO ()
changeIndent i = (modifyIORef indentation (+i))


traceIO :: String -> IO ()
traceIO a = do
    indent <- readIORef indentation
    putStrLn (replicate (2*indent) '.'++ a)

{-# NOINLINE trace #-}
trace :: String -> b -> b
trace a b = unsafePerformIO (traceIO a) `seq` b

debug :: Char -> Matrix RealWorld -> IO ()
-- debug x m  = return ()
debug x m
    = do
      putStrLn (x:replicate 107 '-')
      printMatrix m
