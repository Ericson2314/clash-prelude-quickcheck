module Engn1630.Util.SignalInstances where

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


instance Arbitrary a => Arbitrary (Signal a) where
  arbitrary = fromList <$> infiniteList

  --TODO: what does it mean to shrink an infinite stream
  --shrink x  = [] : (fmap fromList $ shrink $ CLaSH.Prelude.sample x)
