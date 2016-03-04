{-# LANGUAGE OverloadedLists #-}

module Shuffle
    ( fisherYates
    , fisherYatesSeeded
    , fisherYatesST
    , fisherYatesSeededST
    )
where



import           Control.Monad
import           Control.Monad.ST
import           Data.Vector                (MVector, Vector)
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
import           System.Random.TF.Gen
import           System.Random.TF.Init
import           System.Random.TF.Instances



-- | Randomly permute the elements of a 'Vector' using the Fisher-Yates
-- algorithm.
fisherYates :: RandomGen gen => gen -> Vector a -> (Vector a, gen)
fisherYates gen vec = runST (do
    vecMut <- V.thaw vec
    gen' <- fisherYatesST gen vecMut
    vec' <- V.unsafeFreeze vecMut
    pure (vec', gen') )

-- | 'fisherYates' with a specific seed for the RNG.
fisherYatesSeeded :: Int -> Vector a -> Vector a
fisherYatesSeeded seed vec = fst (fisherYates (mkTFGen seed) vec)

-- | In-place 'fisherYates'.
fisherYatesST :: RandomGen gen => gen -> MVector s a -> ST s gen
fisherYatesST gen vec
    | VM.length vec <= 1 = pure gen
fisherYatesST gen vec = do
    let (r, gen') = randomR (0, VM.length vec - 1) gen
    VM.swap vec 0 r
    fisherYatesST gen' (VM.tail vec)

-- | In-place 'fisherYates' with a specific seed for the RNG.
fisherYatesSeededST :: Int -> MVector s a -> ST s ()
fisherYatesSeededST seed vec = void (fisherYatesST (mkTFGen seed) vec)
