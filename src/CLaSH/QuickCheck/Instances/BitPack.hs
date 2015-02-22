module CLaSH.QuickCheck.Instances.BitPack where

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat

import           CLaSH.QuickCheck.Instances.BitVector
import           CLaSH.QuickCheck.Instances.Vec


instance (BitPack (Unsigned a), KnownNat (BitSize (Unsigned a)))
         => Arbitrary (Unsigned a) where
  arbitrary = unpack <$> arbitrary
  shrink    = fmap unpack . shrink . pack

instance (BitPack (Signed a), KnownNat (BitSize (Signed a)))
         => Arbitrary (Signed a) where
  arbitrary = unpack <$> arbitrary
  shrink    = fmap unpack . shrink . pack
