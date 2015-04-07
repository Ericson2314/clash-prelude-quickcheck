module CLaSH.QuickCheck.Instances.Signal () where

import           Data.Bits

import           Control.Applicative

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude.Explicit hiding (lift)
import           CLaSH.Signal
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


instance Arbitrary a => Arbitrary (Signal' clk a) where
  arbitrary = fromList <$> infiniteList

  --TODO: what does it mean to shrink an infinite stream
  --shrink x  = [] : (fmap fromList $ shrink $ CLaSH.Prelude.sample x)
