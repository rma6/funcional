import Test.QuickCheck
import Control.Monad

data Tree a = Nilt |
              Node a (Tree a) (Tree a)
              deriving(Ord, Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree        
arbTree 0 = return Nilt
arbTree n = frequency [(3, return Nilt), (7, liftM3 Node arbitrary (arbTree (n `div` 2)) (arbTree (n `div` 3)))]


--Uma árvore binária com N nós internos tem, nó máximo, N+1 folhas
treeNodes :: Tree t -> Int
treeLeafs :: Tree t -> Int

treeNodes Nilt = 0
treeNodes (Node _ l r) = 1 + (treeNodes l) + (treeNodes r)

treeLeafs Nilt = 0
treeLeafs (Node _ Nilt Nilt) = 1
treeLeafs (Node _ l r) = (treeLeafs l) + (treeLeafs r)

prop_1 :: Arbitrary a => Tree a -> Bool
prop_1 t = treeLeafs t <= treeNodes t + 1