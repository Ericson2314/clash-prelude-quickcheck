module CLaSH.QuickCheck.Instances.Vec where

import           Control.Applicative
import           Control.Monad

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))

import           Unsafe.Coerce (unsafeCoerce)

import           CLaSH.Prelude
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


arb' :: Arbitrary a => UNat n -> Gen (Vec n a)
arb' = \case
  UZero     -> return Nil
  (USucc n) -> (:>) <$> arbitrary <*> arb' n

shrink' :: Arbitrary a => Vec n a -> [Vec n a]
shrink' = \case
  Nil     -> [Nil]
  a :> as -> join $ (\x -> (:> x) <$> shrink a) <$> shrink' as

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec n a) where
  arbitrary = withSNat $ arb' . toUNat
  shrink x  = shrink' x


compare' :: Ord a => Vec n a -> Vec n a -> Ordering
compare' Nil     Nil     = EQ
compare' (x:>xs) (y:>ys) = case compare x y of
  EQ    -> compare' xs $ unsafeCoerce ys
  other -> other
compare' _ _ = error "impossible"

instance (Ord a) => Ord (Vec n a) where
  compare = compare'

--instance Testable a => Testable (a, a) where
