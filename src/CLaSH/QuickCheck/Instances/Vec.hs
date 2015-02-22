{-# LANGUAGE InstanceSigs #-}
module CLaSH.QuickCheck.Instances.Vec where

import           Prelude ()

import           Data.Traversable

import           Control.Applicative

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))

import           Unsafe.Coerce (unsafeCoerce)

import           CLaSH.Prelude hiding (sequence)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


arb' :: Arbitrary a => UNat n -> Gen (Vec n a)
arb' = \case
  UZero     -> return Nil
  (USucc n) -> (:>) <$> arbitrary <*> arb' n

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = withSNat $ arb' . toUNat
  shrink x  = sequence $ shrink <$> x

instance (Ord a) => Ord (Vec n a) where
  compare x y = foldr f EQ $ zipWith compare x y
    where f EQ   keepGoing = keepGoing
          f done _         = done

