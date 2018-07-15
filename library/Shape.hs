{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language ConstraintKinds #-}
module Shape where
import Types

type FlatIndex = Int
class Shape sh where
    inRange :: sh -> sh -> sh -> Bool
    size :: sh -> Int
class Layout l where
    type Index l
    toFlatIndex :: l -> Index l -> FlatIndex
    fromFlatIndex :: l -> Int -> Index l
    extend :: l -> Index l

type LayoutI l = (Layout l, Index l ~ Int)
-- fastest moving index is right to keep with repa
data DIM2 = DIM2 !Int !Int
instance Shape DIM2 where
    {-# INLINE size #-}
    {-# INLINE inRange #-}
    size (DIM2 rowCount columnCount) = rowCount * columnCount
    inRange (DIM2 r0 c0) (DIM2 rm cm) (DIM2 r c)
        = r >= r0 && r < rm && c >= c0 && c < cm
instance Layout DIM2 where
    {-# INLINE toFlatIndex #-}
    {-# INLINE extend #-}
    {-# INLINE fromFlatIndex #-}
    type Index DIM2 = DIM2
    extend s = s
    toFlatIndex (DIM2 columnCount _rowCount) (DIM2 column row)
        = column + row * columnCount
    fromFlatIndex (DIM2 columnCount _rowCount) idx
      = DIM2 rows columns
      where (rows, columns) = idx `divMod` columnCount

newtype RowWise a = RowWise a
instance Layout (RowWise DIM2) where
   type Index (RowWise DIM2) = Int
   {-# INLINE toFlatIndex #-}
   {-# INLINE extend #-}
   {-# INLINE fromFlatIndex #-}
   toFlatIndex _ i = i
   fromFlatIndex _ i = i
   extend (RowWise d) = size (extend d)
data Window = Window !Int !Int !Int !Int
instance Layout (RowWise Window) where
    type Index (RowWise Window) = Int
    {-# INLINE toFlatIndex #-}
    {-# INLINE extend #-}
    {-# INLINE fromFlatIndex #-}
    extend (RowWise (Window _ wRows wCols _)) = wRows * wCols
    toFlatIndex (RowWise (Window offset _ nwincols ncols)) wIdx
      = offset + cols + ncols * rows
      where (rows, cols) = wIdx `divMod` nwincols
    fromFlatIndex (RowWise (Window offset _ nwincols ncols)) idx
      = winRow * nwincols + winCol
      where (winRow, winCol) = (idx - offset) `divMod` ncols

data Column = Column !Int !Int
instance Shape Int where
    {-# INLINE size #-}
    {-# INLINE inRange #-}
    inRange a b v = v >= a && v < b
    size v = v
instance Layout Column where
    type Index Column = Int
    {-# INLINE toFlatIndex #-}
    {-# INLINE extend #-}
    {-# INLINE fromFlatIndex #-}
    toFlatIndex (Column col ncols) row = ncols * row + col
    fromFlatIndex (Column col ncols) idx = (idx - col) `div` ncols
    extend (Column _ ncols) = ncols
data Row = Row !Int !Int
instance Layout Row where
    type Index Row = Int
    {-# INLINE toFlatIndex #-}
    {-# INLINE extend #-}
    {-# INLINE fromFlatIndex #-}
    toFlatIndex (Row offset _) col = offset + col
    fromFlatIndex (Row offset _) idx = idx - offset
    extend (Row _ nrows) = nrows

{-# INLINE unRowWise #-}
unRowWise :: RowWise a -> a
unRowWise (RowWise a) = a

{-# INLINE toRow #-}
toRow :: Int -> (RowWise DIM2) -> Row
toRow row (RowWise (DIM2 nrows ncols)) = Row (row * ncols) nrows
{-# INLINE toCol #-}
toCol :: Int -> (RowWise DIM2) -> Column
toCol row (RowWise (DIM2 _ ncols)) = Column row ncols

{-# INLINE window #-}
window :: (Int, Int) -> (Int, Int) -> RowWise DIM2 -> RowWise Window
window (!bRows, !bCols) (!wRows, !wCols) (RowWise (DIM2 _ !nCols))
  = RowWise (Window offset wRows wCols nCols)
  where offset = bRows * nCols + bCols

{-# INLINE layoutMatrix #-}
layoutMatrix :: (l1 -> l2) -> Matrix l1 s -> Matrix l2 s
layoutMatrix f (Matrix l v) = Matrix (f l) v
