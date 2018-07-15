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
import Data.Foldable


main :: IO ()
main = do
    content <- readFile "resources/sudoku17.txt"   
    let step a acc = do
                        r <- solveSudoku a
                        if r then return (acc+1) else return acc
    n <- foldrM step 0 (lines content )
    print n
    return ()


solveSudoku :: String -> IO Bool
solveSudoku line =  do
    m <- parseSudoku line :: IO (Matrix (RowWise DIM2) RealWorld)
    solve m
    -- done <- S.and $ S.map snd $ G.mstream (mCells m)
    -- return done


{-# INLINE solve #-}
solve ::  Matrix (RowWise DIM2) (PrimState IO) -> IO Bool
solve m = loopSingletons
  where
      loopSingletons = do
          _ <- applySingletons m
          debug 's'

          loopHiddenSingletons
      loopHiddenSingletons = do
          r <- applyHiddenSingletons m
          debug 'h'
          if r
          then loopHiddenSingletons
          else loopPreemptives
      loopPreemptives = do
          r <- applyPreemptives m
          debug 'p'
          if r
          then loopHiddenSingletons
          else  recurse
      recurse = do
          firstUnfixed <- headMaybe (toStream m)
          case firstUnfixed of
              SJust (idx, set) -> shortCutFromTo 1 9 (\i -> doRecursion set idx i m)
              SNothing -> return True
      debug x   = return ()
        -- = do
        --   putStrLn (x:replicate 107 '-')
        --   printMatrix m
          
{-# INLINE doRecursion #-}
doRecursion :: (m ~ IO) => DigitSet -> Int -> Int -> Matrix (RowWise DIM2) (PrimState m) -> m Bool
doRecursion oldSet idx curTry m
    | (mask .&. oldSet) == DigitSet 0 = return False
    | otherwise = do
        vec' <-  G.clone (mCells  m)
        let m' = (Matrix (mLayout m) vec')
        fixCell idx mask m'
        solve m'
    where mask = toDigitSet curTry
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
applySingletons m = mapMatrixM step m
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
    applySet !mask = mapMatrixM (applyMask m mask) m

{-# INLINE applyMask #-}
applyMask :: (PrimMonad m) => Matrix l (PrimState m) -> DigitSet -> Int -> DigitSet -> m Bool
applyMask m !mask !idx !cur
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
                s <- loop state' count set
                return (r || s)
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


{-# INLINE fixCell #-}
fixCell :: forall l m. (PrimMonad m) => Int -> DigitSet -> Matrix l (PrimState m) -> m ()
fixCell i mask m = do
    writeLin m i (mask, True)
    let
        {-# INLINE apply #-}
        apply :: LayoutI li => (RowWise DIM2 -> li) -> m Bool
        apply f = mapMatrixM step (layoutMatrix f mRowWise)
        mRowWise :: Matrix (RowWise DIM2) (PrimState m)
        mRowWise = (Matrix (RowWise (DIM2 9 9)) (mCells m))

        {-# INLINE step #-}
        step :: Int -> DigitSet -> m Bool 
        step i' s = applyMask m mask i' s
        -- DIM2 row column = fromFlatIndex (unRowWise (mLayout m)) i
        -- FIXME: this
        (row, column) = i `divMod` 9
        sRow = (row `div` 3) * 3
        sCol = (column `div` 3) * 3

    _ <- apply (toRow row)
    _ <- apply (toCol column)
    _ <- apply (window (sRow, sCol) (3, 3))
    return ()

{-# INLINE writeLin #-}
writeLin ::  (PrimMonad m) => Matrix l (PrimState m) -> Int -> (DigitSet, Bool) -> m ()
writeLin m idx v = G.write (mCells m) idx v
