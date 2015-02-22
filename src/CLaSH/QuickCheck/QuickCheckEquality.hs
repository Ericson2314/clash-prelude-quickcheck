{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Engn1630.Util.QuickCheckEquality where

import           Data.Bits
import           Data.Maybe
import qualified Data.Map as M
import           Data.Map (Map)

import qualified Test.QuickCheck
import           Test.QuickCheck hiding ((.&.))
