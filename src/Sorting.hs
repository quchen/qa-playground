module Sorting
    ( quicksort
    , slowsort
    , selectionsort
    )
where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.STRef
import           Data.Vector         (MVector, Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM



quicksort :: Ord a => Vector a -> Vector a
quicksort = V.modify quicksortST

quicksortST :: Ord a => MVector s a -> ST s ()
quicksortST vec
    | VM.length vec <= 1 = pure ()
quicksortST vec = do
    (first, second) <- quickPartition vec
    quicksortST first
    quicksortST second

quickPartition :: Ord a => MVector s a -> ST s (MVector s a, MVector s a)
quickPartition vec = do
    pivot <- VM.unsafeRead vec 0
    iRef <- newSTRef 1
    for_ [1 .. VM.length vec - 1] (\focusIx -> do
        focus <- VM.unsafeRead vec focusIx
        when (focus < pivot) (do
            i <- readSTRef iRef
            VM.unsafeSwap vec i focusIx
            modifySTRef' iRef (+1) ))
    pivotDestination <- fmap (subtract 1) (readSTRef iRef)
    VM.unsafeSwap vec 0 pivotDestination
    let (beforePivot, fromPivot) = VM.splitAt pivotDestination vec
    pure (beforePivot, VM.tail fromPivot)



slowsort :: Ord a => Vector a -> Vector a
slowsort = V.modify slowsortST

slowsortST :: Ord a => MVector s a -> ST s ()
slowsortST vec
    | VM.length vec <= 1 = pure ()
slowsortST vec = do
    let middle = VM.length vec `quot` 2
        (firstHalf, secondHalf) = VM.splitAt middle vec
    slowsortST firstHalf
    slowsortST secondHalf
    i <- VM.unsafeRead vec 0
    m <- VM.unsafeRead vec middle
    when (i > m) (VM.unsafeSwap vec 0 middle)
    slowsortST (VM.tail vec)




selectionsort :: Ord a => Vector a -> Vector a
selectionsort = V.modify selectionsortST

selectionsortST :: Ord a => MVector s a -> ST s ()
selectionsortST vec
    | VM.length vec <= 1 = pure ()
selectionsortST vec = do
    minIndex <- do
        candidateValueRef <- newSTRef =<< VM.unsafeRead vec 0
        candidateIx <- newSTRef 0
        for_ [1 .. VM.length vec - 1] (\i -> do
            v <- VM.unsafeRead vec i
            candidateValue <- readSTRef candidateValueRef
            when (v < candidateValue) (do
                writeSTRef candidateValueRef v
                writeSTRef candidateIx i ))
        readSTRef candidateIx
    VM.unsafeSwap vec 0 minIndex
    selectionsortST (VM.tail vec)
