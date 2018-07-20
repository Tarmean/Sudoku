module Example (main) where
import Solver
import Data.ByteString as B
import ByteStringIter
import qualified Data.Vector.Fusion.Stream.Monadic as S
import GHC.Conc (numCapabilities)


main :: IO ()
main = do
    bs <- B.readFile "resources/sudoku17.txt"   

    let
      {-# INLINE step #-}
      step !acc !m = do
          r <- solve m
          if r
          then return (acc + 1)
          else return acc

    n <- withStreamM numCapabilities bs (S.foldlM' step (0::Int)) (+)


    print n
    return ()

