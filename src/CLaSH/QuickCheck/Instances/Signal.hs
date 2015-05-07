module CLaSH.QuickCheck.Instances.Signal () where

import Test.QuickCheck

import CLaSH.Prelude
import CLaSH.Prelude.Explicit


instance Arbitrary a => Arbitrary (Signal' clk a) where
  arbitrary = fromList <$> infiniteList

  --TODO: what does it mean to shrink an infinite stream
  --shrink x  = [] : (fmap fromList $ shrink $ CLaSH.Prelude.sample x)
