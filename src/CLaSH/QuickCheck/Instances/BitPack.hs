module CLaSH.QuickCheck.Instances.BitPack () where

import Test.QuickCheck

import CLaSH.Prelude

import CLaSH.QuickCheck.Instances.BitVector ()
import CLaSH.QuickCheck.Instances.Vec ()


instance (BitPack (Unsigned a), KnownNat (BitSize (Unsigned a)))
         => Arbitrary (Unsigned a) where
  arbitrary = unpack <$> arbitrary
  shrink    = fmap unpack . shrink . pack

instance (BitPack (Signed a), KnownNat (BitSize (Signed a)))
         => Arbitrary (Signed a) where
  arbitrary = unpack <$> arbitrary
  shrink    = fmap unpack . shrink . pack
