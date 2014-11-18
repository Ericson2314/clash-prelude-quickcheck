module Engn1630.Util.BitInstances where

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat

import qualified CLaSH.Sized.Vector as V

iso1 :: Bool -> Bit
iso1 = \case False -> 0
             True  -> 1

iso2 :: Bit -> Bool
iso2 = \case 0 -> False
             1 -> True
             _ -> error"impossible"

instance KnownNat n => Arbitrary (BitVector n) where
  arbitrary = pack <$> (V.repeat <$> iso1 <$> arbitrary :: Gen (Vec n Bit))
--  shrink l  = fmap iso1 $ shrink $ iso2 l


inv :: Bits a => a -> a
inv = complement

