module CLaSH.QuickCheck.Instances.BitVector () where

import Test.QuickCheck

import CLaSH.Prelude


iso1 :: Bool -> Bit
iso1 = \case False -> 0
             True  -> 1

iso2 :: Bit -> Bool
iso2 = \case 0 -> False
             _ -> True

instance KnownNat n => Arbitrary (BitVector n) where
  arbitrary = fmap pack $ sequence $ CLaSH.Prelude.repeat $ iso1 <$> arbitrary
  shrink v  = fmap pack vs
    where vs :: [Vec n Bit]
          vs = (fmap . fmap) iso1 $ sequence $ shrink <$> iso2 <$> unpack v
