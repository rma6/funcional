data Tree a = Nilt |
              Node a (Tree a) (Tree a)
              deriving(Ord, Eq, Show)

type Set a = Tree a

--instance Eq a => Eq (Set a) where
--    (==) = eqSet

--instance Ord a => Ord (Set a) where
--    (<=) = leqSet

empty :: Set a
empty = Nilt

sing :: a -> Set a
sing a = Node a Nilt Nilt

--checa se um valor pentence ao conjunto
memSet :: Ord a => Set a -> a -> Bool
memSet Nilt _ = False
memSet (Node a tl tr) v | v==a = True
                     | v<a = memSet tl v
                     | v>a = memSet tr v

--insere um elemento no conjunto
insert :: Ord a => Set a -> a -> Set a
insert Nilt v = (Node v Nilt Nilt)
insert (Node a tl tr) v | memSet (Node a tl tr) v = (Node a tl tr)
                        | v<a = Node a (insert tl v) tr
                        | v>a = Node a tl (insert tr v)

--função auxiliar de remove. No caso do nó a ser removido ter 2 filho, utiliza-se essa função para retornar a árvore à direita do nó a ser removido com o valor mínimo dela agora na raiz
minRightTree :: Ord a => Set a -> Set a
minRightTree (Node a tl tr) = Node (findMinRightTree tr) tl (remove tr (findMinRightTree tr))

--Retorna o menor elemento da árvore da direita
findMinRightTree :: Ord a => Set a -> a
findMinRightTree (Node a Nilt Nilt) = a
findMinRightTree (Node a Nilt tr) = a
findMinRightTree (Node a tl tr) = findMinRightTree tl

--remove um elemento do conjunto
remove :: Ord a => Set a -> a -> Set a
remove (Node a Nilt Nilt) _ = Nilt
remove (Node a tl tr) v | not (memSet (Node a tl tr) v) = (Node a tl tr)
                        | v<a = Node a (remove tl v) tr
                        | v>a = Node a tl (remove tr v)
                        | v==a && tl/=Nilt && tr/=Nilt = (minRightTree (Node a tl tr))
                        | v==a && tl/=Nilt && tr==Nilt = tl
                        | v==a && tl==Nilt && tr/=Nilt = tr
                        | v==a && tl==Nilt && tr==Nilt = Nilt


makeSet :: Ord a => [a] -> Set a
makeSet [] = Nilt
makeSet (a:ax)

set2List :: Ord a => Set a -> [a]
set2List (Nilt) = []
set2List (Node a tl tr) = (set2List tl)++[a]++(set2List tr)

card :: Ord a => Set a -> Int
card s = length (set2List s)