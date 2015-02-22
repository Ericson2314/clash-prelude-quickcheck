{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CLaSH.QuickCheck.QuickCheckEquality where

import           Data.Bits
import           Data.Maybe
import qualified Data.Map as M
import           Data.Map (Map)

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))
