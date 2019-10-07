data Tree a = Nilt |
              Node a (Tree a) (Tree a)
              deriving(Ord, Eq, Show)

type Set a = Tree a

--instance Show Set a where
--    Show (Set a) = 

--checa se um valor pentence ao conjunto
has :: (Eq a, Ord a) => Set a -> a -> Bool
has Nilt _ = False
has (Node a tl tr) v | v==a = True
                     | v<a = has tl v
                     | v>a = has tr v

--insere um elemento no conjunto
insert :: (Eq a, Ord a) => Set a -> a -> Set a
insert Nilt v = (Node v Nilt Nilt)
insert (Node a tl tr) v | has (Node a tl tr) v = (Node a tl tr)
                        | v<a = Node a (insert tl v) tr
                        | v>a = Node a tl (insert tr v)

--função auxiliar de remove. No caso do nó a ser removido ter 2 filho, utiliza-se essa função para retornar a árvore à direita do nó a ser removido com o valor mínimo dela agora na raiz
minRightTree :: (Eq a, Ord a) => Set a -> Set a
minRightTree (Node a tl tr) = Node (findMinRightTree tr) tl (remove tr (findMinRightTree tr))

--Retorna o menor elemento da árvore da direita
findMinRightTree :: (Eq a, Ord a) => Set a -> a
findMinRightTree (Node a Nilt Nilt) = a
findMinRightTree (Node a Nilt tr) = a
findMinRightTree (Node a tl tr) = findMinRightTree tl

--remove um elemento do conjunto
remove :: (Eq a, Ord a) => Set a -> a -> Set a
remove (Node a Nilt Nilt) _ = Nilt
remove (Node a tl tr) v | not (has (Node a tl tr) v) = (Node a tl tr)
                        | v<a = Node a (remove tl v) tr
                        | v>a = Node a tl (remove tr v)
                        | v==a && tl/=Nilt && tr/=Nilt = (minRightTree (Node a tl tr))
                        | v==a && tl/=Nilt && tr==Nilt = tl
                        | v==a && tl==Nilt && tr/=Nilt = tr
                        | v==a && tl==Nilt && tr==Nilt = Nilt

--Set2List :: (Ord a) => Set a -> [a]