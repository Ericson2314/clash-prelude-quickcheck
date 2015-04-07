module CLaSH.QuickCheck.Instances.Vec () where

import           Prelude ()

import           Data.Traversable

import           Control.Applicative

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))

import           Unsafe.Coerce (unsafeCoerce)

import           CLaSH.Prelude hiding (sequence)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = sequence $ repeat arbitrary
  shrink x  = sequence $ shrink <$> x
