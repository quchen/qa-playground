{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where



import           Control.Applicative
import           Data.Monoid
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Data.Vector.Algorithms.Tim as Tim
import           System.Random.TF.Init      (mkTFGen)
import           Test.SmallCheck.Series     as SC
import           Test.Tasty
import           Test.Tasty.HUnit           as HU
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC

import qualified Shuffle                    as Subject
import qualified Sort                       as Subject



main :: IO ()
main = defaultMain (options testsuite)

options :: TestTree -> TestTree
options = quickcheckOptions . smallcheckOptions
  where
    quickcheckOptions =
          localOption (QuickCheckShowReplay False)
        . localOption (QuickCheckTests 1000)
        . localOption (QuickCheckMaxSize 100)
    smallcheckOptions =
          localOption (SmallCheckDepth 5)

testsuite :: TestTree
testsuite =
    testGroup "Testsuite"
        [ testGroup "Sorting algorithms"
            [ testGroup "Quicksort" quicksortTests
            , testGroup "Slowsort" slowsortTests
            , testGroup "Selection sort" selectionsortTests
            , testGroup "Bubblesort" bubblesortTests
            ]
        , testGroup "Shuffling algorithms"
            [ testGroup "Fisher-Yates" fisherYatesTests ]
        ]

quicksortTests :: [TestTree]
quicksortTests = sortingTests arbitrary Subject.quicksort

-- Test generators are scaled down so this won't dominate the test time taken
slowsortTests :: [TestTree]
slowsortTests = sortingTests (scale (min 24) arbitrary) Subject.slowsort

selectionsortTests :: [TestTree]
selectionsortTests = sortingTests arbitrary Subject.selectionsort

bubblesortTests :: [TestTree]
bubblesortTests = sortingTests arbitrary Subject.bubblesort

instance Serial m a => Serial m (Vector a) where
    series = fmap V.fromList series

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList arbitrary

sortingTests
    :: Gen (Vector Int) -- ^ Quickcheck generator, passed as argument so
                        -- Slowsort doesn't take forever with its atrociously
                        -- bad performance :-)
    -> (Vector Int -> Vector Int)
    -> [TestTree]
sortingTests gen f =
    [ testGroup "QuickCheck"
        [ QC.testProperty
            "Leaves sorted input invariant"
            (QC.forAll (fmap sort gen) (f ~~ id))
        , QC.testProperty
            "Leaves length invariant"
            (QC.forAll gen (length . f ~~ length))
        , QC.testProperty
            "Reversal of input doesn't matter"
            (QC.forAll gen (f . V.reverse ~~ f))
        , QC.testProperty
            "Is idempotent"
            (QC.forAll gen (f . f ~~ f))
        , QC.testProperty
            "Agrees with library sort function"
            (QC.forAll gen (f ~~ sort))
        , QC.testProperty
            "All vectors are palindromes"
            (QC.forAll gen (f ~~ V.reverse . sort))
        , QC.testProperty
            "Result is in ascending order"
            (QC.forAll gen (\xs -> V.length xs >= 2 QC.==>
                let isAscending ys = V.and (V.zipWith (<=) ys (V.tail ys))
                in isAscending (f xs) ))
        ]
    , testGroup "SmallCheck"
        [ SC.testProperty
            "Only [] maps to []"
            (SC.existsUnique (V.null . f))
        , SC.testProperty
            "Only one input sorts to a singleton vector"
            (SC.existsUnique (\xs -> V.length (f xs) == 1))
        , SC.testProperty
            "Sorted list contains the smallest element of the input"
            (\xs -> not (V.null xs) SC.==>
                V.find (== V.head (f xs)) xs == Just (V.minimum xs))
        ]
    , testGroup "HUnit"
        [ testCase
            "sort [1] = []"
            (assertEqual "" [1] (f []))
        , testCase
            "Example case: sort [7,9,6,5,3,2,1,8,0,4] = [0..9]"
            (let expected = [0..9::Int]
                 actual = f [7,9,6,5,3,2,1,8,0,4]
             in assertEqual "" expected actual )
        , testCaseSteps "Sorting powers of two, in steps" (\step -> do
            step "Prepare input"
            -- Ignore that Haskell is lazy for a moment :-)
            let expected = [1,2,4,8,16,32,64,128,256,512]
                actual = f [4,64,2,128,8,32,512,16,256,1]
            step "Perform test"
            assertEqual "" expected actual)
        ]
    ]

sort :: Ord a => Vector a -> Vector a
sort = V.modify Tim.sort

(~~), (/~)
    :: (Show b, Eq b)
     => (a -> b)
     -> (a -> b)
     -> a
     -> QC.Property
 -- | Pointwise equality of functions
(~~) = liftA2 (===)
-- | Pointwise inequality of functions
f /~ g = \x ->
    let fx = f x; gx = g x
    in counterexample (show fx <> " == " <> show gx) (fx /= gx)
infix 4 ~~
infix 4 /~

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> QC.Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)



fisherYatesTests :: [TestTree]
fisherYatesTests =
    [ QC.testProperty
        "Length unchanged"
        (\seed vec -> (V.length . f seed ~~ V.length) vec)
    , QC.testProperty
        "Sum unchanged"
        (\seed vec -> (V.sum . f seed ~~ V.sum) vec)
    , QC.testProperty
        "Product unchanged"
        (\seed vec -> (V.product . f seed ~~ V.product) vec)
    , QC.testProperty
        "Permutation of input"
        (\seed vec -> (sort . f seed ~~ sort) vec)
    , QC.testProperty
        "Different seeds yield different output"
        (\seed1 seed2 -> seed1 /= seed2 QC.==>
            (f seed1 /~ f seed2) [1..1000::Int])
    , QC.testProperty
        "fisherYatesSeeded matches its ST version"
        (\seed (vec :: Vector Int) -> (===)
            (Subject.fisherYatesSeeded seed vec)
            (V.modify (Subject.fisherYatesSeededInplace seed) vec))
    , QC.testProperty
        "Generator is modified in the process"
        (\seed (vec :: Vector Int) -> V.length vec > 1 QC.==>
            let gen = mkTFGen seed
                (_vec, gen') = Subject.fisherYates (mkTFGen seed) vec
            in show gen /== show gen' )
    ]
  where
    f :: Int -> Vector Int -> Vector Int
    f = Subject.fisherYatesSeeded
