{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Heap as Heap

import qualified Sorting as Subject



main :: IO ()
main = do
    let quickcheckOptions =
              localOption (QuickCheckShowReplay False)
            . localOption (QuickCheckTests 1000)
            . localOption (QuickCheckMaxSize 100)
    defaultMain . quickcheckOptions $
        testGroup "Sorting"
            [ testGroup "Quicksort"
                [ prop_quicksort_heapsort
                , prop_quicksort_mergesort
                ]
            , testGroup "Slowsort"
                [ prop_slowsort_heapsort
                , prop_slowsort_mergesort
                ]
            , testGroup "Selection sort"
                [ prop_selectionsort_heapsort
                , prop_selectionsort_mergesort
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

prop_quicksort_heapsort, prop_quicksort_mergesort :: TestTree
prop_quicksort_heapsort = testProperty
    "agrees with library's heapsort"
    (\xs -> let _ = xs :: [Int]
            in denotationallyEqual Subject.quicksort (V.modify Heap.sort) xs)
prop_quicksort_mergesort = testProperty
    "agrees with library's mergesort"
    (\xs -> let _ = xs :: [Int]
            in denotationallyEqual Subject.quicksort (V.modify Merge.sort) xs)


prop_slowsort_heapsort, prop_slowsort_mergesort :: TestTree
-- Test generators are scaled down so this won't dominate the test time taken
prop_slowsort_heapsort = testProperty
    "agrees with library's heapsort"
    (forAll (scale (min 32) arbitrary)
            (\xs -> let _ = xs :: [Int]
                    in denotationallyEqual Subject.slowsort (V.modify Heap.sort) xs))
prop_slowsort_mergesort = testProperty
    "agrees with library's mergesort"
    (forAll (scale (min 32) arbitrary)
            (\xs -> let _ = xs :: [Int]
                    in denotationallyEqual Subject.slowsort (V.modify Merge.sort) xs))


prop_selectionsort_heapsort, prop_selectionsort_mergesort :: TestTree
prop_selectionsort_heapsort = testProperty
    "agrees with library's heapsort"
    (\xs -> let _ = xs :: [Int]
            in denotationallyEqual Subject.selectionsort (V.modify Heap.sort) xs)
prop_selectionsort_mergesort = testProperty
    "agrees with library's mergesort"
    (\xs -> let _ = xs :: [Int]
            in denotationallyEqual Subject.selectionsort (V.modify Merge.sort) xs)
