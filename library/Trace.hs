module Trace (trace, debug) where
import PrintMatrix
-- import Debug.Trace
trace a b = b

debug x m  = return ()
-- debug x m
--     = do
--       putStrLn (x:replicate 107 '-')
--       printMatrix m
