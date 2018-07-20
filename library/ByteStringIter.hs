module ByteStringIter (withStreamM) where
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.ByteString.Internal
import Control.Concurrent.Async as A
import GHC.Word
import Foreign.ForeignPtr       (withForeignPtr)
import Foreign.Ptr
import Foreign.Storable
import Data.Bits

import Types
import WriteCell

data ParserState = ParserState !Int !(Ptr Word8) !Int

-- digitSets :: UN.Vector DigitSet
-- digitSets = V.iterateN 9 (`shiftL` 1) (DigitSet 1)

-- {-# INLINE step #-}
step :: ParserState -> IO (S.Step ParserState Matrix)
step (ParserState !offset !ptr !len)
    | len - offset < 81 = return S.Done
    | otherwise = do
        !vec <- G.replicate 81 (DigitSet ((2^(9::Int))-1), False)
        let m = Matrix vec
        let loop !cur
              | cur >= 81 = return ()
              | otherwise = do
                  digit <- peek (ptr `plusPtr` (offset + cur)) :: IO Word8
                  case digit of
                    46 ->  return ()
                    i | i >= 49 && i <= 58 -> fixCellLin cur (DigitSet 1 `shiftL` fromIntegral (digit - 49 )) m
                    _ -> error ("Illegal character " ++ show digit)
                  loop (cur + 1)
        loop 0
        return (S.Yield m (ParserState (offset + lineWidth) ptr len))
lineWidth :: Int
lineWidth = 82



{-# INLINE withStreamM #-}
withStreamM :: (Show r) => Int -> ByteString -> (S.Stream IO Matrix -> IO r) -> (r -> r -> r) -> IO r
withStreamM threads (PS !fp !off !len)  f combine  = withForeignPtr fp $ \p -> do
    let
        processChunk i = f (S.Stream step (ParserState (chunkLen*(i-1)+off)  p end))
          where end = if i == threads then len else chunkLen * i + off
        chunkLen = ((len `div` lineWidth) `div` threads) * lineWidth
        offsets = [1..threads]
    ls <- A.mapConcurrently processChunk offsets
    return (foldr1 combine ls)
