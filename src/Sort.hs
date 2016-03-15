module Sort
    ( quicksort
    , quicksortBy

    , slowsort
    , slowsortBy

    , selectionsort
    , selectionsortBy

    , bubblesort
    , bubblesortBy
) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.STRef
import           Data.Vector         (MVector)
import qualified Data.Vector.Mutable as VM

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import qualified Data.Vector as V



-- | Sort a list in-place with the /Quicksort/ algorithm.
--
-- >>> V.modify quicksort [3,7,6,1,9,2,8,4,5,10]
-- [1,2,3,4,5,6,7,8,9,10]
quicksort :: Ord a => MVector s a -> ST s ()
quicksort = quicksortBy compare

-- | Non-overloaded version of 'quicksort'.
quicksortBy :: (a -> a -> Ordering) -> MVector s a -> ST s ()
quicksortBy _cmp vec
    | VM.length vec <= 1 = pure ()
quicksortBy cmp vec = do
    (first, second) <- quickPartitionBy cmp vec
    quicksortBy cmp first
    quicksortBy cmp second

quickPartitionBy
    :: (a -> a -> Ordering)
    -> MVector s a
    -> ST s (MVector s a, MVector s a)
quickPartitionBy cmp vec = do
    pivot <- VM.read vec 0
    iRef <- newSTRef 1
    for_ [1 .. VM.length vec - 1] (\focusIx -> do
        focus <- VM.read vec focusIx
        when (cmp focus pivot /= GT) (do
            i <- readSTRef iRef
            VM.swap vec i focusIx
            modifySTRef' iRef (+1) ))
    pivotDestination <- fmap (subtract 1) (readSTRef iRef)
    VM.swap vec 0 pivotDestination
    let (beforePivot, fromPivot) = VM.splitAt pivotDestination vec
    pure (beforePivot, VM.tail fromPivot)



-- | Sort a list in-place with the absurdly inefficient /Slowsort/ algorithm.
--
-- >>> V.modify slowsort [7,5,9,8,2,4,1,10,6,3]
-- [1,2,3,4,5,6,7,8,9,10]
slowsort :: Ord a => MVector s a -> ST s ()
slowsort = slowsortBy compare

-- | Non-overloaded version of 'slowsort'.
slowsortBy :: (a -> a -> Ordering) -> MVector s a -> ST s ()
slowsortBy _cmp vec
    | VM.length vec <= 1 = pure ()
slowsortBy cmp vec = do
    let middle = VM.length vec `quot` 2
        (firstHalf, secondHalf) = VM.splitAt middle vec
    slowsortBy cmp firstHalf
    slowsortBy cmp secondHalf
    sort2By cmp vec 0 middle
    slowsortBy cmp (VM.tail vec)



-- | Sort a list with the /Selection Sort/ algorithm.
--
-- >>> V.modify selectionsort [3,5,2,10,7,8,9,4,1,6]
-- [1,2,3,4,5,6,7,8,9,10]
selectionsort :: Ord a => MVector s a -> ST s ()
selectionsort = selectionsortBy compare

-- | Non-overloaded version of 'selectionsort'.
selectionsortBy :: (a -> a -> Ordering) -> MVector s a -> ST s ()
selectionsortBy _cmp vec
    | VM.length vec <= 1 = pure ()
selectionsortBy cmp vec = do
    jMin <- newSTRef 0
    for_ [0 .. VM.length vec - 1] $ \i -> do
        writeSTRef jMin i
        for_ [i+1 .. VM.length vec - 1] $ \j -> do
            vj <- VM.read vec j
            vJMin <- VM.read vec =<< readSTRef jMin
            when (vj `cmp` vJMin == LT)
                 (writeSTRef jMin j)
        VM.swap vec i =<< readSTRef jMin




-- | Sort a list with the inefficient /Bubblesort/ algorithm.
--
-- >>> V.modify bubblesort [5,10,9,6,3,2,7,4,1,8]
-- [1,2,3,4,5,6,7,8,9,10]
bubblesort :: Ord a => MVector s a -> ST s ()
bubblesort = bubblesortBy compare

-- | Non-overloaded version of 'bubblesort'.
bubblesortBy :: (a -> a -> Ordering) -> MVector s a -> ST s ()
bubblesortBy cmp vec =
    for_ [0 .. VM.length vec - 2] (\i ->
        for_ [i+1 .. VM.length vec - 1] (\j ->
            sort2By cmp vec i j) )
{-# ANN bubblesortBy "HLint: ignore Avoid lambda" #-}

-- | Modify the 'MVector' so that the values at the given indices
-- are in order.
sort2By :: (a -> a -> Ordering) -> MVector s a -> Int -> Int -> ST s ()
sort2By cmp vec i j = do
    vi <- VM.read vec i
    vj <- VM.read vec j
    when (cmp vi vj == GT) (VM.swap vec i j)
