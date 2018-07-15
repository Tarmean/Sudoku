{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language TypeFamilies #-}
{-# Language ScopedTypeVariables #-}

module Example (main) where
import Types
import StreamSlice
import ParseSudoku
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic.Mutable as G
import Data.Bits
import Control.Monad.Primitive
import Control.Monad
import Shape


main :: IO ()
main = do
    content <- readFile "resources/sudoku17.txt"   
    -- let step acc a = do
    --                     r <- solveSudoku a
    --                     if r then return (acc+1) else return acc
    -- let loop (x:xs) i = do
    --                         b <- solveSudoku x
    --                         if not b
    --                         then return i
    --                         else loop xs (i+1)
    -- n <- loop (lines content) 0
    mapM_ solveSudoku (take 3000 $ lines content)
    -- print n
    -- mapM solveSudoku (lines content)
    -- i <- foldM step (0::Int) (take 3000 $ lines content)
    -- print i
    return ()


solveSudoku :: String -> IO Bool
solveSudoku line =  do
    m <- parseSudoku line :: IO (Matrix (RowWise DIM2) RealWorld)
    solve m
    return True
    -- done <- S.and $ S.map snd $ G.mstream (mCells m)
    -- return done


{-# INLINE solve #-}
solve ::  Matrix (RowWise DIM2) (PrimState IO) -> IO ()
solve m = loopSingletons
  where
      loopSingletons = do
          r <- applySingletons m
          debug
          if r
          then loopSingletons
          else loopHiddenSingletons
      loopHiddenSingletons = do
          r <- applyHiddenSingletons m
          debug
          if r
          then loopHiddenSingletons
          else loopPreemptives
      loopPreemptives = do
          r <- applyPreemptives m
          debug
          if r
          then loopSingletons
          else  return ()
      -- recurse m 
      debug =  return ()
        -- = do
          -- putStrLn ('s':replicate 107 '-')
          -- printMatrix m
          
printMatrix :: Matrix (RowWise DIM2) RealWorld -> IO ()
printMatrix m =
     forM_ [0..8] $ \i -> do
         rowString <- formatRow $ getRow i
         putStrLn rowString
  where
    getRow i = S.map (\(_,d,b) -> (d, b)) $ toFullStream $ layoutMatrix (toRow i) m
    formatRow s = S.foldl (\acc (c, b) -> acc . mark b . shows c) id s <*> pure ""
    mark b = if b then ('|' :) else ('.' :)

{-# INLINE allRegions #-}
allRegions :: Monad m => (forall l. (LayoutI l) => Matrix l (PrimState m) -> m Bool) -> Matrix (RowWise DIM2) (PrimState m) -> m Bool
allRegions f m = do
   a <- shortCutFromTo 0 8 (\i -> f $ layoutMatrix (toRow i) m)
   if a then return True
   else do
       b <- shortCutFromTo 0 8 (\i -> f $ layoutMatrix (toCol i) m)
       if b then return True
       else
           shortCutFromTo 0 2 (\i -> shortCutFromTo 0 2 (\j -> 
                   f $ layoutMatrix (window (3*i, 3*j) (3,3)) m))


{-# INLINE applySingletons #-}
applySingletons :: forall m. (PrimMonad m) => Matrix (RowWise DIM2) (PrimState m) -> m Bool
applySingletons m = shortcutMatrixM step m
  where
    {-# INLINE step #-}
    step !idx !set
      | isSingleton set =
            fixCell idx set m >> return True
      | otherwise = return False
{-# INLINE isSingleton #-}
isSingleton :: DigitSet -> Bool
isSingleton s = popCount s == 1

{-# INLINE applyPreemptives #-}
applyPreemptives :: (PrimMonad m) => Matrix (RowWise DIM2) (PrimState m) -> m Bool
applyPreemptives = allRegions preemptivePass

{-# INLINE preemptivePass #-}
preemptivePass :: (LayoutI l, PrimMonad m) => Matrix l (PrimState m) -> m Bool
preemptivePass m = searchPreemptives applySet (S.map snd $ toStream m)
  where
    {-# INLINE applySet #-}
    applySet !mask = shortcutMatrixM (applyMask m mask) m
{-# INLINE applyMask #-}
applyMask :: (PrimMonad m) => Matrix l (PrimState m) -> DigitSet -> Int -> DigitSet -> m Bool
applyMask m mask idx cur
    | cur `isSubsetOf` mask = return False
    | cur' == cur = return False
    | isSingleton cur' = do
        fixCell idx cur' m
        return True
    | otherwise = do
        writeLin m idx (cur', False) 
        return  True
    where cur' = cur \\ mask
{-# INLINE searchPreemptives #-}
searchPreemptives :: Monad m => (DigitSet -> m Bool) -> S.Stream m DigitSet -> m Bool
searchPreemptives applySet (S.Stream step s0) = loop s0 0 (DigitSet 0)
  where
    {-# INLINE loop #-}
    loop !state !count !set = do
        m <- step state
        case m of
            S.Done -> return False
            S.Skip state' -> loop state' count set
            S.Yield a state' -> do
                r <- (check state' (count+1) (a .|. set))
                if r
                then return True
                else loop state' count set
    {-# INLINE check #-}
    check !state' !count' !set'
        | count' == popCount set' = applySet set'
        | count' >= 4 = return False
        | otherwise = loop state' count' set'

data HiddenSingleton = None | OnlyAt !Int | Multiple

{-# INLINE applyHiddenSingletons #-}
applyHiddenSingletons :: forall m. (PrimMonad m) => Matrix (RowWise DIM2) (PrimState m) -> m Bool
applyHiddenSingletons m = allRegions hiddenSingletonPass m
  where

    {-# INLINE hiddenSingletonPass #-}
    hiddenSingletonPass :: (LayoutI l) => Matrix l (PrimState m) -> m Bool
    hiddenSingletonPass m' =  shortCutFromTo 1 9 (step m')

    {-# INLINE step #-}
    step :: (LayoutI l) => Matrix l (PrimState m) -> Int -> m Bool
    step !m' !i = do
        let !mask = toDigitSet i 
        h <- searchHiddenSingleton mask (toStream m')
        case h of
           OnlyAt idx -> do
               writeLin m idx (mask, False)
               fixCell idx mask m 
               return True
           _ -> return False

{-# INLINE shortCutFromTo #-}
shortCutFromTo :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Bool
shortCutFromTo zero end p = loop zero
  where
    loop i
      | i >= end = return False
      | otherwise = do
        r <- p i
        if r
        then return True
        else loop (i+1)
     
{-# INLINE searchHiddenSingleton #-}
searchHiddenSingleton :: Monad m => DigitSet -> S.Stream m (Int, DigitSet) -> m HiddenSingleton
searchHiddenSingleton !mask s = S.foldl step None s
   where
     step Multiple _ = Multiple
     step a (idx, set)
       | (set .&. mask) /= DigitSet 0 = case a of
           None -> OnlyAt idx
           _ -> Multiple
       | otherwise = a


{-# INLINE (\\) #-}
{-# INLINE isSubsetOf #-}
isSubsetOf :: DigitSet -> DigitSet -> Bool
isSubsetOf a b = (b .|. a) == b
(\\) :: DigitSet -> DigitSet -> DigitSet
(\\) a b = complement b .&. a


{-# INLINE mapLayout #-}
mapLayout 
    :: (LayoutI li, PrimMonad m)
    => (DigitSet -> DigitSet) -> Matrix l (PrimState m) -> (l -> li) -> m ()
mapLayout f m !l = mapMatrix f (layoutMatrix l m)

{-# INLINE fixCell #-}
fixCell :: forall l m. (PrimMonad m) => Int -> DigitSet -> Matrix l (PrimState m) -> m ()
fixCell i mask m = do
    writeLin m i (mask, True)
    let
        {-# INLINE apply #-}
        apply :: LayoutI li => (RowWise DIM2 -> li) -> m ()
        apply f = mapMatrixM step (layoutMatrix f mRowWise)
        mRowWise :: Matrix (RowWise DIM2) (PrimState m)
        mRowWise = (Matrix (RowWise (DIM2 9 9)) (mCells m))

        {-# INLINE step #-}
        step :: Int -> DigitSet -> m ()
        step i' s = void (applyMask m mask i' s)
        -- DIM2 row column = fromFlatIndex (unRowWise (mLayout m)) i
        -- FIXME: this
        (row, column) = i `divMod` 9
        sRow = (row `div` 3) * 3
        sCol = (column `div` 3) * 3

    apply (toRow row)
    apply (toCol column)
    apply (window (sRow, sCol) (3, 3))

-- {-# INLINE write #-}
-- setCell :: (Layout l, PrimMonad m) => Matrix l (PrimState m) -> Index l -> (DigitSet, Bool) -> m ()
-- setCell (Matrix l vec) idx v = G.write vec (toFlatIndex l idx) v


{-# INLINE writeLin #-}
writeLin ::  (PrimMonad m) => Matrix l (PrimState m) -> Int -> (DigitSet, Bool) -> m ()
writeLin m idx v = G.write (mCells m) idx v
