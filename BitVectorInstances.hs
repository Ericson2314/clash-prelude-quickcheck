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

import           Engn1630.Util.BitInstances
import           Engn1630.Util.VecInstances

instance (BitPack (Unsigned a), KnownNat (BitSize (Unsigned a)))
         => Arbitrary (Unsigned a) where
  arbitrary = unpack <$> arbitrary
  shrink    = fmap unpack . shrink . pack
