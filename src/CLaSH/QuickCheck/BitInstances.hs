{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CLaSH.QuickCheck.BitInstances where

import           Prelude            hiding (sequence)

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (sequence, lift)
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
  arbitrary = fmap pack $ sequence $ V.repeat $ iso1 <$> arbitrary
--  shrink l  = fmap iso1 $ shrink $ iso2 l


inv :: Bits a => a -> a
inv = complement

sequence :: Applicative m => Vec n (m a) -> m (Vec n a)
sequence = \case
  Nil     -> pure Nil
  m :> ms -> (:>) <$> m <*> sequence ms
