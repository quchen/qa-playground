module Main (main) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Heap as Heap

import Sorting

main :: IO ()
main = do
    let quickcheckOptions =
              localOption (QuickCheckShowReplay False)
            . localOption (QuickCheckTests 1000)
            . localOption (QuickCheckMaxSize 100)
    defaultMain . quickcheckOptions $
        testGroup "Sorting"
            [ testGroup "Quicksort"
                [ testProperty "agrees with library's Heapsort" prop_quicksort_heapsort
                , testProperty "agrees with library's Mergesort" prop_quicksort_mergesort
                ]
            , testGroup "Slowsort"
                [ testProperty "agrees with library's Heapsort" prop_slowsort_heapsort
                , testProperty "agrees with library's Mergesort" prop_slowsort_mergesort
                ]
            , testGroup "Selection sort"
                [ testProperty "agrees with library's Heapsort" prop_selectionsort_heapsort
                , testProperty "agrees with library's Mergesort" prop_selectionsort_mergesort
                ]
            ]



denotationallyEqual
    :: (Arbitrary a, Eq a)
    => (Vector a -> Vector a)
    -> (Vector a -> Vector a)
    -> [a]
    -> Bool
denotationallyEqual f g = \xs -> let vec = V.fromList xs
                                 in f vec == g vec

prop_quicksort_heapsort, prop_quicksort_mergesort :: [Int] -> Bool
prop_quicksort_heapsort = denotationallyEqual quicksort (V.modify Heap.sort)
prop_quicksort_mergesort = denotationallyEqual quicksort (V.modify Merge.sort)

prop_slowsort_heapsort, prop_slowsort_mergesort :: [Int] -> Bool
-- take 50 so slowsort doesn't slow down the tests too much :-)
prop_slowsort_heapsort = denotationallyEqual slowsort (V.modify Heap.sort) . take 20
prop_slowsort_mergesort = denotationallyEqual slowsort (V.modify Merge.sort) . take 20

prop_selectionsort_heapsort, prop_selectionsort_mergesort :: [Int] -> Bool
prop_selectionsort_heapsort = denotationallyEqual selectionsort (V.modify Heap.sort)
prop_selectionsort_mergesort = denotationallyEqual selectionsort (V.modify Merge.sort)