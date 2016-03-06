{-# LANGUAGE OverloadedLists #-}

module Shuffle
    ( -- * Fisher-Yates algorithm
      fisherYates'
    , fisherYates
    )
where



import           Control.Monad
import           Data.Vector                (MVector)
import qualified Data.Vector.Mutable        as VM
import           System.Random.TF.Gen
import           System.Random.TF.Init
import           System.Random.TF.Instances
import Control.Monad.Primitive (PrimMonad, PrimState)

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import qualified Data.Vector as V



-- | In-place 'fisherYates' with a specific seed for the RNG.
--
-- >>> V.modify (fisherYates' 123) [1..10]
-- [5,7,1,3,2,8,10,4,9,6]
fisherYates'
    :: PrimMonad m
    => Int
    -> MVector (PrimState m) a
    -> m ()
fisherYates' seed vec = void (fisherYates (mkTFGen seed) vec)

-- | Shuffle a 'Vector' with a specific seed for the RNG using the
-- <https://en.wikipedia.org/wiki/Fisher-Yates Fisher-Yates algorithm>.
-- Returns the modified generator.
--
-- >>> V.modify (void . fisherYates (mkTFGen 123)) [1..10]
-- [5,7,1,3,2,8,10,4,9,6]
fisherYates
    :: (PrimMonad m, RandomGen gen)
    => gen
    -> MVector (PrimState m) a
    -> m gen
fisherYates gen vec
    | VM.length vec <= 1 = pure gen
fisherYates gen vec = do
    let (r, gen') = randomR (0, VM.length vec - 1) gen
    VM.swap vec 0 r
    fisherYates gen' (VM.tail vec)
