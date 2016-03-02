{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Vector as V
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
            [ testGroup "Quicksort" (sortTests arbitrary Subject.quicksort)

            -- Test generators are scaled down so this won't dominate the test time taken
            , testGroup "Slowsort" (sortTests (scale (min 32) arbitrary) Subject.slowsort)

            , testGroup "Selection sort" (sortTests arbitrary Subject.selectionsort)
            ]

sortTests
    :: Gen [Int]
    -> (V.Vector Int -> V.Vector Int)
    -> [TestTree]
sortTests gen algorithm =
    [ testProperty
        "Is idempotent"
        (forAll gen (algorithm . algorithm ~~ algorithm))
    , testProperty
        "Leaves length invariant"
        (forAll gen (length . algorithm ~~ length))
    , testProperty
        "Agrees with library sort function"
        (forAll gen (algorithm ~~ V.modify Heap.sort))
    ]
  where
    infixl 2 ~~
    f ~~ g = \xs -> let vec = V.fromList xs
                    in f vec === g vec
