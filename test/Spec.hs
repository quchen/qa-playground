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
                [ test prop_quicksort_heapsort
                , test prop_quicksort_mergesort
                ]
            , testGroup "Slowsort"
                [ test prop_slowsort_heapsort
                , test prop_slowsort_mergesort
                ]
            , testGroup "Selection sort"
                [ test prop_selectionsort_heapsort
                , test prop_selectionsort_mergesort
                ]
            ]



-- | Test case grouped with a descriptive name.
data Testcase a = Test String a

test :: Testable a => Testcase a -> TestTree
test (Test descr prop) = testProperty descr prop



denotationallyEqual
    :: (Arbitrary a, Eq a)
    => (Vector a -> Vector a)
    -> (Vector a -> Vector a)
    -> [a]
    -> Bool
denotationallyEqual f g = \xs -> let vec = V.fromList xs
                                 in f vec == g vec

prop_quicksort_heapsort, prop_quicksort_mergesort :: Testcase ([Int] -> Bool)
prop_quicksort_heapsort = Test
    "agrees with library's heapsort"
    (denotationallyEqual Subject.quicksort (V.modify Heap.sort))
prop_quicksort_mergesort = Test
    "agrees with library's mergesort"
    (denotationallyEqual Subject.quicksort (V.modify Merge.sort))


prop_slowsort_heapsort, prop_slowsort_mergesort :: Testcase Property
-- Test generators are scaled down so this won't dominate the test time taken
prop_slowsort_heapsort = Test
    "agrees with library's heapsort"
    (forAll (scale (min 32) arbitrary)
            (\xs -> let _ = xs :: [Int]
                    in denotationallyEqual Subject.slowsort (V.modify Heap.sort) xs))
prop_slowsort_mergesort = Test
    "agrees with library's mergesort"
    (forAll (scale (min 32) arbitrary)
            (\xs -> let _ = xs :: [Int]
                    in denotationallyEqual Subject.slowsort (V.modify Merge.sort) xs))
-- take 32 so slowsort doesn't slow down the tests too much :-)


prop_selectionsort_heapsort, prop_selectionsort_mergesort :: Testcase ([Int] -> Bool)
prop_selectionsort_heapsort = Test
    "agrees with library's heapsort"
    (denotationallyEqual Subject.selectionsort (V.modify Heap.sort))
prop_selectionsort_mergesort = Test
    "agrees with library's mergesort"
    (denotationallyEqual Subject.selectionsort (V.modify Merge.sort))