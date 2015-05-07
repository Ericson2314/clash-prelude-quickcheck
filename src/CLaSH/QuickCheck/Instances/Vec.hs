module CLaSH.QuickCheck.Instances.Vec () where

import Test.QuickCheck

import CLaSH.Prelude


instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = sequence $ CLaSH.Prelude.repeat arbitrary
  shrink x  = sequence $ shrink <$> x
