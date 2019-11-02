import Data.Set
import Test.QuickCheck
import Control.Monad

--A ⊆ B if and only if A ∩ B = A
prop_1 :: Arbitrary a => Ord a => Set a -> Set a -> Bool
prop_1 a b = (a `isSubsetOf` b) == (a `intersection` b == a)
--if and only if is equivalent to check equality of 2 Bool

--produto cartesiano A × (B ∪ C) = (A × B) ∪ (A × C)
prop_2 :: Arbitrary a => Ord a => Set a -> Set a -> Set a -> Bool
prop_2 a b c = a `cartesianProduct` (b `union` c) == (a `cartesianProduct`b) `union` (a `cartesianProduct` c)

--if A ⊆ B then A \ B = ∅
prop_3 :: Arbitrary a => Ord a => Set a -> Set a -> Bool
prop_3 a b = not (a `isSubsetOf` b) || a \\ b == empty
-- A->B is equivalent to ~A||B

--A ≠ B -> A \ B ≠ B \ A
prop_4 :: Arbitrary a => Ord a => Set a -> Set a -> Bool
prop_4 a b = (a == b) || (a \\ b /= b \\ a)

-- | A × B | = | B × A | = | A | * | B |
prop_5 :: Arbitrary a => Ord a => Set a -> Set a -> Bool
prop_5 a b = size (a `cartesianProduct` b) == size (b `cartesianProduct` a) && size (a `cartesianProduct` b) == size a * size b