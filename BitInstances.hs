module Engn1630.Util.BitInstances where

import           Data.Bits

import qualified Test.QuickCheck
import           Test.QuickCheck

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat

iso1 = \case False -> L
             True  -> H

iso2 = \case L -> False
             H -> True

instance Arbitrary Bit where
  arbitrary = iso1 <$> arbitrary
  shrink l = fmap iso1 $ shrink $ iso2 l


inv :: Bits a => a -> a
inv = complement


