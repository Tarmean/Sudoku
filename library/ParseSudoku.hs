{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}
module ParseSudoku (parseSudoku) where
import Types
import Shape
import Data.Char
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import Control.Monad.Primitive

digit :: Int -> DigitSet
digit i = DigitSet (2^(i-1))

wildcard :: DigitSet
wildcard = DigitSet 511

parseEntry :: Char -> (DigitSet, Bool)
parseEntry '.' = (wildcard, False)
parseEntry c = (digit (digitToInt c), False)

parseLine :: String -> [(DigitSet, Bool)]
parseLine = map parseEntry

parseSudoku :: (PrimMonad m) => String -> m (Matrix (RowWise DIM2) (PrimState m))
parseSudoku s = do
    vec <- G.unstream . B.fromListN 81 . parseLine $ s
    return (Matrix (RowWise (DIM2 9 9)) vec)

-- without using bundles directly this would be horrible:
-- parseSudoku :: (G.Vector v DigitSet, PrimMonad m) => proxy v -> String -> m (G.Mutable v (PrimState m) DigitSet)
-- parseSudoku _ = G.thaw . G.fromList . parseLine
