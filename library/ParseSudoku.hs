{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}
module ParseSudoku (parseSudoku) where
import Types
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

parseSudoku :: (PrimMonad m) => String -> m (Matrix (PrimState m))
parseSudoku s = do
    vec <- G.unstream . B.fromListN 81 . parseLine $ s
    return (Matrix vec)
