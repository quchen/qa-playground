{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Control.Applicative
import           Control.Monad.ST
import           Data.Monoid
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Data.Vector.Algorithms.Tim as Tim
import           System.Random.TF.Init      (mkTFGen)
import           Test.Tasty
import           Test.Tasty.HUnit           as HU
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC

import           Orphans                    ()

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
quicksortTests = sortingTests arbitrary (V.modify Subject.quicksort)

-- Test generators are scaled down so this won't dominate the test time taken
slowsortTests :: [TestTree]
slowsortTests = sortingTests (scale (min 24) arbitrary) (V.modify Subject.slowsort)

selectionsortTests :: [TestTree]
selectionsortTests = sortingTests arbitrary (V.modify Subject.selectionsort)

bubblesortTests :: [TestTree]
bubblesortTests = sortingTests arbitrary (V.modify Subject.bubblesort)

sortingTests
    :: Gen (Vector Int) -- ^ Quickcheck generator, passed as argument so
                        -- Slowsort doesn't take forever with its atrociously
                        -- bad performance :-)
    -> (Vector Int -> Vector Int) -- ^ Sorting algorithm
    -> [TestTree]
sortingTests gen algorithm =
    [ testGroup "QuickCheck"
        [ QC.testProperty
            "Leaves sorted input invariant"
            (QC.forAll (fmap libSort gen) (algorithm ~~ id))
        , QC.testProperty
            "Leaves length invariant"
            (QC.forAll gen (length . algorithm ~~ length))
        , QC.testProperty
            "Reversal of input doesn't matter"
            (QC.forAll gen (algorithm . V.reverse ~~ algorithm))
        , QC.testProperty
            "Is idempotent"
            (QC.forAll gen (algorithm . algorithm ~~ algorithm))
        , QC.testProperty
            "Agrees with library sort function"
            (QC.forAll gen (algorithm ~~ libSort))
        , QC.testProperty
            "All vectors are palindromes"
            (QC.forAll gen (algorithm ~~ V.reverse . algorithm))
        , QC.testProperty
            "Result is in ascending order"
            (QC.forAll gen (\xs -> V.length xs >= 2 QC.==>
                let isAscending ys = V.and (V.zipWith (<=) ys (V.tail ys))
                in isAscending (algorithm xs) ))
        ]
    , testGroup "SmallCheck"
        [ SC.testProperty
            "Only [] maps to []"
            (SC.existsUnique (V.null . algorithm))
        , SC.testProperty
            "Only one input sorts to a singleton vector"
            (SC.existsUnique (\xs -> V.length (algorithm xs) == 1))
        , SC.testProperty
            "Sorted list contains the smallest element of the input"
            (\xs -> not (V.null xs) SC.==>
                V.find (== V.head (algorithm xs)) xs == Just (V.minimum xs))
        ]
    , testGroup "HUnit"
        [ testCase
            "sort [1] = []"
            (assertEqual "" [1] (algorithm []))
        , testCase
            "Example case: sort [7,9,6,5,3,2,1,8,0,4] = [0..9]"
            (let expected = [0..9::Int]
                 actual = algorithm [7,9,6,5,3,2,1,8,0,4]
             in assertEqual "" expected actual )
        , testCaseSteps "Sorting powers of two, in steps" (\step -> do
            step "Prepare input"
            -- Ignore that Haskell is lazy for a moment :-)
            let expected = [1,2,4,8,16,32,64,128,256,512]
                actual = algorithm [4,64,2,128,8,32,512,16,256,1]
            step "Perform test"
            assertEqual "" expected actual)
        ]
    ]

libSort :: Ord a => Vector a -> Vector a
libSort = V.modify Tim.sort

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
        (\seed -> V.length . f seed ~~ V.length)
    , QC.testProperty
        "Sum unchanged"
        (\seed -> V.sum . f seed ~~ V.sum)
    , QC.testProperty
        "Product unchanged"
        (\seed -> V.product . f seed ~~ V.product)
    , QC.testProperty
        "Permutation of input"
        (\seed -> libSort . f seed ~~ libSort)
    , QC.testProperty
        "Different seeds yield different output"
        (\seed1 seed2 -> seed1 /= seed2 QC.==>
            (f seed1 /~ f seed2) [1..1000::Int])
    , QC.testProperty
        "RNG is modified"
        (\seed (vec :: Vector Int) -> V.length vec > 1 QC.==>
            let gen = mkTFGen seed
                gen' = runST (V.thaw vec >>= Subject.fisherYates (mkTFGen seed))
            in show gen /== show gen' )
    ]
  where
    f :: Int -> Vector Int -> Vector Int
    f seed = V.modify (Subject.fisherYates' seed)
