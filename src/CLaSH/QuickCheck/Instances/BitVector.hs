module CLaSH.QuickCheck.Instances.BitVector () where

import           Prelude ()

import           Data.Bits
import           Data.Traversable

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude hiding (sequence, lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat

import qualified CLaSH.Sized.Vector as V


iso1 :: Bool -> Bit
iso1 = \case False -> 0
             True  -> 1

iso2 :: Bit -> Bool
iso2 = \case 0 -> False
             _ -> True

instance KnownNat n => Arbitrary (BitVector n) where
  arbitrary = fmap pack $ sequence $ V.repeat $ iso1 <$> arbitrary
  shrink v  = fmap pack x
    where x :: [Vec n Bit]
          x = (fmap . fmap) iso1 $ sequence $ shrink <$> iso2 <$> unpack v
