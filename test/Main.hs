{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where



import           Control.Applicative
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as Heap
import           Test.SmallCheck.Series      as SC
import           Test.Tasty
import           Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck       as QC
import           Test.Tasty.SmallCheck       as SC

import qualified Sorting                     as Subject



main :: IO ()
main = do
    let options = quickcheckOptions . smallcheckOptions
          where
            quickcheckOptions =
                  localOption (QuickCheckShowReplay False)
                . localOption (QuickCheckTests 1000)
                . localOption (QuickCheckMaxSize 100)
            smallcheckOptions =
                  localOption (SmallCheckDepth 5)
    defaultMain . options $
        testGroup "Sorting"
            [ testGroup "Quicksort" (sortingTests arbitraryVector Subject.quicksort)

            -- Test generators are scaled down so this won't dominate the test time taken
            , testGroup "Slowsort" (sortingTests (scale (min 24) arbitraryVector) Subject.slowsort)

            , testGroup "Selection sort" (sortingTests arbitraryVector Subject.selectionsort)

            , testGroup "Bubblesort" (sortingTests arbitraryVector Subject.bubblesort)
            ]

arbitraryVector :: Gen (Vector Int)
arbitraryVector = fmap V.fromList arbitrary

instance Serial m a => Serial m (Vector a) where
    series = fmap V.fromList series

sortingTests
    :: Gen (Vector Int)
    -> (Vector Int -> Vector Int)
    -> [TestTree]
sortingTests gen f =
    [ testGroup "QuickCheck"
        [ QC.testProperty
            "Leaves sorted input invariant"
            (QC.forAll (fmap sort gen)
                    (f ~~ id))
        , QC.testProperty
            "Leaves length invariant"
            (QC.forAll gen
                    (length . f ~~ length))
        , QC.testProperty
            "Reversal of input doesn't matter"
            (QC.forAll gen
                    (f . V.reverse ~~ f))
        , QC.testProperty
            "Is idempotent"
            (QC.forAll gen
                    (f . f ~~ f))
        , QC.testProperty
            "Agrees with library sort function"
            (QC.forAll gen
                    (f ~~ sort))
        ]
    , testGroup "SmallCheck"
        [ SC.testProperty
            "Only [] maps to []"
            (SC.existsUnique (\xs -> f xs == []))
        , SC.testProperty
            "Sorted list contains the smallest element of the input"
            (SC.forAll (\xs ->
                not (V.null xs) SC.==>
                    V.find (== V.head (f xs)) xs == Just (V.minimum xs)))
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
     -> (a -> QC.Property)
(~~) = liftA2 (===)
infixl 2 ~~
