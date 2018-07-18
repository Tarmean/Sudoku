module ByteStringIter (withStreamM) where
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.ByteString.Internal
import GHC.Word
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Primitive
import Data.Bits

import Types
import WriteCell

data ParserState = ParserState !Int !(Ptr Word8) !Int

-- digitSets :: UN.Vector DigitSet
-- digitSets = V.iterateN 9 (`shiftL` 1) (DigitSet 1)

{-# INLINE step #-}
step :: ParserState -> IO (S.Step ParserState (Matrix RealWorld))
step (ParserState offset ptr len)
    | len - offset < 81 = return S.Done
    | otherwise = do
        vec <- G.replicate 81 (DigitSet ((2^(9::Int))-1), False)
        let m = Matrix vec
        let loop !cur
              | cur >= 81 = return ()
              | otherwise = do
                  digit <- peek (ptr `plusPtr` (offset + cur)) :: IO Word8
                  case digit of
                    46 ->  return ()
                    i | i >= 49 && i <= 58 -> fixCell cur (DigitSet 1 `shiftL` fromIntegral (digit - 49 )) m
                    _ -> error ("Illegal character " ++ [toEnum$ fromIntegral digit])
                  loop (cur + 1)
        loop 0
        return (S.Yield (Matrix vec) (ParserState (offset + 81 + newLineWidth) ptr len))
newLineWidth :: Int
newLineWidth = 1

{-# INLINE withStreamM #-}
withStreamM :: ByteString -> (S.Stream IO (Matrix RealWorld) -> IO r) -> IO r
withStreamM (PS fp off len)  f = withForeignPtr fp $ \p ->
    f (S.Stream step (ParserState off p len))
