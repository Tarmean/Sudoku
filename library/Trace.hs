module Trace (trace, debug) where
import PrintMatrix
import Control.Monad.Primitive
import Types
-- import Debug.Trace

trace :: String -> b -> b
trace a b = b

debug :: Char -> Matrix RealWorld -> IO ()
debug x m  = return ()
-- debug x m
--     = do
--       putStrLn (x:replicate 107 '-')
-- --       printMatrix m
