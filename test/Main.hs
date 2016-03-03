{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Control.Applicative
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as Heap
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Sorting                     as Subject



main :: IO ()
main = do
    let quickcheckOptions =
              localOption (QuickCheckShowReplay False)
            . localOption (QuickCheckTests 1000)
            . localOption (QuickCheckMaxSize 100)
    defaultMain . quickcheckOptions $
        testGroup "Sorting"
            [ testGroup "Quicksort" (sortingTests arbitraryVector Subject.quicksort)

            -- Test generators are scaled down so this won't dominate the test time taken
            , testGroup "Slowsort" (sortingTests (scale (min 24) arbitraryVector) Subject.slowsort)

            , testGroup "Selection sort" (sortingTests arbitraryVector Subject.selectionsort)

            , testGroup "Bubblesort" (sortingTests arbitraryVector Subject.bubblesort)
            ]

arbitraryVector :: Gen (Vector Int)
arbitraryVector = fmap V.fromList arbitrary

sortingTests
    :: Gen (Vector Int)
    -> (Vector Int -> Vector Int)
    -> [TestTree]
sortingTests gen algorithm =
    [ testProperty
        "Leaves sorted input invariant"
        (forAll (fmap sort gen)
                (algorithm ~~ id))
    , testProperty
        "Leaves length invariant"
        (forAll gen
                (length . algorithm ~~ length))
    , testProperty
        "Reversal of input doesn't matter"
        (forAll gen
                (algorithm . V.reverse ~~ algorithm))
    , testProperty
        "Is idempotent"
        (forAll gen
                (algorithm . algorithm ~~ algorithm))
    , testProperty
        "Agrees with library sort function"
        (forAll gen
                (algorithm ~~ V.modify Heap.sort))
    ]
  where
    sort = V.modify Heap.sort

-- | Pointwise equality of functions
(~~) :: (Show b, Eq b)
     => (a -> b)
     -> (a -> b)
     -> (a -> Property)
(~~) = liftA2 (===)
infixl 2 ~~
