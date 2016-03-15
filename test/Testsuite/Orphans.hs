{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where



import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Test.SmallCheck.Series as SC
import           Test.Tasty.QuickCheck  as QC



instance Serial m a => Serial m (Vector a) where
    series = fmap V.fromList series

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList arbitrary
    shrink = map V.fromList . shrink . V.toList
