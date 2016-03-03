{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Control.Applicative
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as Heap
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
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
sortingTests gen f =
    [ testGroup "Quickcheck"
        [ testProperty
            "Leaves sorted input invariant"
            (forAll (fmap sort gen)
                    (f ~~ id))
        , testProperty
            "Leaves length invariant"
            (forAll gen
                    (length . f ~~ length))
        , testProperty
            "Reversal of input doesn't matter"
            (forAll gen
                    (f . V.reverse ~~ f))
        , testProperty
            "Is idempotent"
            (forAll gen
                    (f . f ~~ f))
        , testProperty
            "Agrees with library sort function"
            (forAll gen
                    (f ~~ sort))
        ]
    , testGroup "HUnit"
        [ testCase
            "Empty vector"
            (assertEqual "Sorting nothing does nothing" [] (f []))
        , testCase
            "Example case: sort [7,9,6,5,3,2,1,8,0,4] = [0..9]"
            (let expected = [0..9::Int]
                 actual = f [7,9,6,5,3,2,1,8,0,4]
             in assertEqual "" expected actual )
        , testCaseSteps "Sorting powers of two" (\step -> do
                step "Prepare input"
                -- Ignore that Haskell is lazy for a moment :-)
                let expected = V.fromList (take 10 (iterate (*2) 1))
                    actual = f (V.fromList (map ((2::Int)^) [0..9::Int]))
                step "Perform test"
                assertEqual "" expected actual)
        ]
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
