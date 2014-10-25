{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Engn1630.Util.BruteBoolean where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Bits
import           Data.Maybe
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Word

--import           Debug.Trace

import qualified Test.QuickCheck
import           Test.QuickCheck    hiding ((.&.))

import           CLaSH.Prelude      hiding (lift)
import           CLaSH.Sized.Vector
import           CLaSH.Promoted.Nat


nand a b = not $ a .&. b
nor  a b = not $ a .|. b


data Expr gate
  = Arg Word
  | Gate gate (Expr gate) (Expr gate)
  deriving (Eq, Ord, Show)

-- can use arg + 1 of each gate type
type Counters gate = Map gate Word

bruteExpr :: (Ord gate, Arbitrary gate) => Word -> Counters gate -> Gen (Expr gate)
bruteExpr numArgs  = evalStateT (brute numArgs)

brute :: forall gate. (Ord gate, Arbitrary gate)
      => Word -> StateT (Counters gate) Gen (Expr gate)
brute numArgs = r
  where tryBrute :: StateT (Counters gate) (MaybeT Gen) (Expr gate)
        tryBrute = do
          (choice1 :: Maybe gate) <- lift (lift arbitrary :: MaybeT Gen (Maybe gate))
          case choice1 of
            Nothing -> lift (lift $ Arg . (`mod` numArgs) <$> arbitrary :: MaybeT Gen (Expr gate))
            (Just op) -> bruteOp op

        bruteOp :: gate -> StateT (Counters gate) (MaybeT Gen) (Expr gate)
        bruteOp op = do

          -- TODO don't lookup map twice
          do guard . M.member op =<< get
             let f = \case
                   0 -> Nothing
                   c -> Just $ c - 1
             modify $ M.update f op

          let node :: StateT (Counters gate) Gen (Expr gate)
              node = Gate op <$> r <*> r

          mapStateT lift node

        -- recur, trying until something works
        r :: StateT (Counters gate) Gen (Expr gate)
        r = do
          s <- get
          let f :: Gen (Maybe (Expr gate, Counters gate))
              f = runMaybeT $ runStateT tryBrute s
          (a, s') <- lift $ whileNothing f
          put {- $ traceShowId -} s'
          return a


whileNothing :: Monad m => m (Maybe a) -> m a
whileNothing m = m >>= \case
  Nothing -> whileNothing m
  Just a  -> return a



data NXCell :: Nat -> Nat -> Nat -> * where
  Body :: SNat a -> SNat n -> SNat x -> Expr Gate -> NXCell a n x
  deriving (Show)

-- hard coding other shits for now
instance forall a n x.  (KnownNat a, KnownNat n, KnownNat x)
         => Arbitrary (NXCell a n x) where

  arbitrary = withSNat $ withSNat $ withSNat f
    where f :: SNat a -> SNat n -> SNat x -> Gen (NXCell a n x)
          f a n x = fmap (Body a n x) $ bruteExpr aw $
                    M.fromList [(Nand, nw), (Xor, xw)]
            where (aw :: Word) = fromIntegral $ snatToInteger a
                  (nw :: Word) = fromIntegral $ snatToInteger n
                  (xw :: Word) = fromIntegral $ snatToInteger x
  shrink _ = []



data Gate
  = Nand
  | Xor
  deriving (Eq, Ord, Show)

isoGate1 = \case
  True  -> Nand
  False -> Xor

isoGate2 = \case
  Nand -> True
  Xor  -> False

instance Arbitrary Gate where
  arbitrary = isoGate1 <$> arbitrary
  shrink l = fmap isoGate1 $ shrink $ isoGate2 l



interp :: (Word -> Bool) -> Expr Gate -> Bool
interp indexArg = \case
  Arg  n       -> indexArg n
  Gate g e1 e2 -> r e1 `f` r e2
    where f = case g of
            Nand -> nand
            Xor  -> xor
  where r = interp indexArg

interpVec :: (KnownNat a) => Vec a Bool -> NXCell a n x -> Bool
interpVec args (Body _ _ _ body) = interp (args !) body
