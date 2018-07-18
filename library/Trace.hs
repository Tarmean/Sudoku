{-# LANGUAGE CPP #-}
module Trace (trace, debug, changeIndent, traceIO, tindent) where
import PrintMatrix
import Control.Monad.Primitive
import Types
import System.IO.Unsafe
import Data.IORef

-- #define TRACE
#ifdef TRACE
{-# NOINLINE indentation #-}
indentation :: IORef Int
indentation = unsafePerformIO (newIORef 0)

{-# INLINE changeIndent #-}
changeIndent :: Int -> IO ()
changeIndent i = (modifyIORef indentation (+i))


traceIO :: String -> IO ()
traceIO a = do
    indent <- readIORef indentation
    putStrLn (replicate (2*indent) '.'++ a)

{-# NOINLINE trace #-}
trace :: String -> b -> b
trace a b = unsafePerformIO (traceIO a) `seq` b

{-# INLINE debug #-}
debug :: Char -> Matrix RealWorld -> IO ()
-- debug x m  = return ()
debug x m
    = do
      putStrLn (x:replicate 107 '-')
      printMatrix m

tindent :: String -> Int -> a -> a
tindent s i = trace (replicate (i * 2) ' ' ++ s )

#endif
#ifndef TRACE
{-# INLINE debug #-}
{-# INLINE traceIO #-}
{-# INLINE tindent #-}
{-# INLINE changeIndent #-}
{-# INLINE trace #-}
debug :: Char -> Matrix RealWorld -> IO ()
debug x m  = return ()
traceIO :: [Char] ->  IO ()
traceIO x  = return ()
changeIndent :: Int -> IO ()
changeIndent i = return ()
trace :: String -> b -> b
trace a b = b
tindent :: String -> Int -> a -> a
tindent s i a = a
#endif
