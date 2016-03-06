module Sort
    ( quicksort
    , slowsort
    , selectionsort
    , bubblesort
    )
where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.STRef
import           Data.Vector         (MVector)
import qualified Data.Vector.Mutable as VM

-- $setup
-- >>> import qualified Data.Vector as V



-- | Sort a list in-place with the /Quicksort/ algorithm.
--
-- >>> V.modify quicksort [3,7,6,1,9,2,8,4,5,10]
-- [1,2,3,4,5,6,7,8,9,10]
quicksort :: Ord a => MVector s a -> ST s ()
quicksort vec
    | VM.length vec <= 1 = pure ()
quicksort vec = do
    (first, second) <- quickPartition vec
    quicksort first
    quicksort second

quickPartition :: Ord a => MVector s a -> ST s (MVector s a, MVector s a)
quickPartition vec = do
    pivot <- VM.read vec 0
    iRef <- newSTRef 1
    for_ [1 .. VM.length vec - 1] (\focusIx -> do
        focus <- VM.read vec focusIx
        when (focus < pivot) (do
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
slowsort vec
    | VM.length vec <= 1 = pure ()
slowsort vec = do
    let middle = VM.length vec `quot` 2
        (firstHalf, secondHalf) = VM.splitAt middle vec
    slowsort firstHalf
    slowsort secondHalf
    sort2 vec 0 middle
    slowsort (VM.tail vec)

-- | Sort a list with the /Selection Sort/ algorithm.
--
-- >>> V.modify selectionsort [3,5,2,10,7,8,9,4,1,6]
-- [1,2,3,4,5,6,7,8,9,10]
selectionsort :: Ord a => MVector s a -> ST s ()
selectionsort vec
    | VM.length vec <= 1 = pure ()
selectionsort vec = do
    i <- minIndex vec
    VM.swap vec 0 i
    selectionsort (VM.tail vec)

minIndex :: Ord a => MVector s a -> ST s Int
minIndex vec = do
    candidateValueRef <- newSTRef =<< VM.read vec 0
    candidateIx <- newSTRef 0
    for_ [1 .. VM.length vec - 1] (\i -> do
        v <- VM.read vec i
        candidateValue <- readSTRef candidateValueRef
        when (v < candidateValue) (do
            writeSTRef candidateValueRef v
            writeSTRef candidateIx i ))
    readSTRef candidateIx

-- | Sort a list with the inefficient /Bubblesort/ algorithm.
--
-- >>> V.modify bubblesort [5,10,9,6,3,2,7,4,1,8]
-- [1,2,3,4,5,6,7,8,9,10]
bubblesort :: Ord a => MVector s a -> ST s ()
bubblesort vec =
    for_ [0 .. VM.length vec - 2] (\i ->
        for_ [i+1 .. VM.length vec - 1] (\j -> sort2 vec i j) )

-- | Modify the 'MVector' so that the values at the given indices
-- are in order.
sort2 :: Ord a => MVector s a -> Int -> Int -> ST s ()
sort2 vec i j = do
    vi <- VM.read vec i
    vj <- VM.read vec j
    when (vi > vj) (VM.swap vec i j)
