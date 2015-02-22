module CLaSH.QuickCheck.MiscProperties where

commutivity :: Eq a => (a -> a -> a) -> (a -> a -> Bool)
commutivity f = \a b -> f a b == f b a

associativity :: Eq a => (a -> a -> a) -> (a -> a -> a -> Bool)
associativity f = \a b c -> f a (f b c) == f (f a b) c
