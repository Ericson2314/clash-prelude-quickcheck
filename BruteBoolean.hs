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

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))


nand a b = not $ a .&. b
nor  a b = not $ a .|. b


data Expr
  = Arg Word
  | Nand Expr Expr
  | Xor  Expr Expr
  deriving (Eq, Ord, Show)

interp :: [Bool] -> Expr -> Bool
interp l = \case
  Arg  n     -> l !! (fromIntegral n)
  Nand e1 e2 -> r e1 `nand` r e2
  Xor  e1 e2 -> r e1 `xor`  r e2

  where r = interp l


bruteExpr :: Word -> Word -> Word -> Gen Expr
bruteExpr numArgs numNands numXors = evalStateT (brute numArgs) (numNands, numXors)

type Counters = (Word, Word)

brute :: Word -> StateT Counters Gen Expr
brute numArgs = r
  where tryBrute :: StateT Counters (MaybeT Gen) Expr
        tryBrute = do
          (choice1 :: Word) <- lift (lift $ (`mod` 2) <$> arbitrary :: MaybeT Gen Word)
          case choice1 of
            0 -> lift (lift $ Arg . (`mod` numArgs) <$> arbitrary :: MaybeT Gen Expr)
            1 -> bruteOp False
--            2 -> bruteOp True

        bruteOp :: Bool -> StateT Counters (MaybeT Gen) Expr
        bruteOp opT = do
          modify $ \(nands , xors) -> case opT of
            True  -> (nands - 1 , xors)
            False -> (nands     , xors - 1)

          -- check count
          do (nands, xors) <- get
             guard $ nands >= 0 && xors >= 0

          let op = case opT of
                False -> Nand
                True  -> Xor

          let node :: StateT Counters Gen Expr
              node = op <$> r <*> r

          mapStateT lift node

        r :: StateT Counters Gen Expr
        r = do
          s <- get
          let f :: Gen (Maybe (Expr, Counters))
              f = runMaybeT $ runStateT tryBrute s
          (a, s') <- lift $ whileNothing f
          put s'
          return a


whileNothing :: Monad m => m (Maybe a) -> m a
whileNothing m = m >>= \case
  Nothing -> whileNothing m
  Just a  -> return a

--instance Arbitrary Expr where
