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
treeNodes Nilt = 0
treeNodes (Node _ l r) = 1 + (treeNodes l) + (treeNodes r)

treeLeafs :: Tree t -> Int
treeLeafs Nilt = 0
treeLeafs (Node _ Nilt Nilt) = 1
treeLeafs (Node _ l r) = (treeLeafs l) + (treeLeafs r)

prop_1 :: Arbitrary a => Tree a -> Bool
prop_1 t = treeLeafs t <= treeNodes t + 1

--O número máximo de nós de uma árvore binária de altura h é 2^(h+1)−1
treeHeight_plus_one :: Tree t -> Int
treeHeight_plus_one Nilt = 0
treeHeight_plus_one (Node _ l r) = 1 + (max (treeNodes l) (treeNodes r))

--Pela definição uma árvore apenas contendo a raiz tem altura 0, portanto é necessário fazer uma correção
treeHeight :: Tree t -> Int
treeHeight t = (treeHeight_plus_one t) - 1

prop_2 :: Arbitrary a => Tree a -> Bool
prop_2 t = treeNodes t <= 2^(treeHeight t + 1)-1

--Uma árvore binária completa, com mais de um nó, com l folhas  tem n=2l-1 nós
isFull :: Tree t -> Bool
isFull Nilt = False
isFull (Node _ Nilt Nilt) = True
isFull (Node _ _ Nilt) = False
isFull (Node _ Nilt _) = False
isFull (Node _ l r) = isFull l && isFull r

prop_3 :: Arbitrary a => Tree a -> Bool
prop_3 t = not((isFull t)) || (treeNodes t == 2*(treeLeafs t)+1)
-- A->B === ~A||B