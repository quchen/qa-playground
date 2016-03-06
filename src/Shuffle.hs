{-# LANGUAGE OverloadedLists #-}

module Shuffle
    ( -- * Fisher-Yates algorithm
      fisherYatesSeeded
    , fisherYates
    , fisherYatesSeededInplace
    , fisherYatesInplace
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
import Control.Monad.Primitive (PrimMonad, PrimState)

-- $setup
-- >>> :set -XOverloadedLists



-- | Shuffle a 'Vector' with a specific seed for the RNG using the
-- <https://en.wikipedia.org/wiki/Fisher-Yates Fisher-Yates algorithm>.
--
-- >>> fisherYatesSeeded 123 [1..10]
-- [5,7,1,3,2,8,10,4,9,6]
--
fisherYatesSeeded :: Int -> Vector a -> Vector a
fisherYatesSeeded seed vec = fst (fisherYates (mkTFGen seed) vec)

-- | Shuffle a 'Vector' using the provided 'RandomGen'.
fisherYates :: RandomGen gen => gen -> Vector a -> (Vector a, gen)
fisherYates gen vec = runST (do
    vecMut <- V.thaw vec
    gen' <- fisherYatesInplace gen vecMut
    vec' <- V.unsafeFreeze vecMut
    pure (vec', gen') )

-- | In-place 'fisherYates'.
fisherYatesInplace
    :: (PrimMonad m, RandomGen gen)
    => gen
    -> MVector (PrimState m) a
    -> m gen
fisherYatesInplace gen vec
    | VM.length vec <= 1 = pure gen
fisherYatesInplace gen vec = do
    let (r, gen') = randomR (0, VM.length vec - 1) gen
    VM.swap vec 0 r
    fisherYatesInplace gen' (VM.tail vec)

-- | In-place 'fisherYates' with a specific seed for the RNG.
fisherYatesSeededInplace
    :: PrimMonad m
    => Int
    -> MVector (PrimState m) a
    -> m ()
fisherYatesSeededInplace seed vec = void (fisherYatesInplace (mkTFGen seed) vec)
