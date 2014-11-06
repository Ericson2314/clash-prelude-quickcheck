{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Engn1630.Util.BitVectorInstances where

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat

instance (BitVector a, KnownNat (BitSize a)) => Arbitrary a where
  arbitrary = fromBV <$> arbitrary
  shrink    = fmap fromBV . shrink . toBV
