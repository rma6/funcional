data Tree a = Nilt |
              Node a (Tree a) (Tree a)
              deriving(Ord, Eq, Show)

type Set a = Tree a

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

--cria um conjunto a partir de uma lista
makeSet :: Ord a => [a] -> Set a
makeSet l = makeSetSeed empty l

--função auxiliar para makeSet
makeSetSeed :: Ord a => Set a -> [a] -> Set a
makeSetSeed s [] = s
makeSetSeed s (a:ax) = makeSetSeed (insert s a) ax

--converte um conjunto para uma lista
set2List :: Ord a => Set a -> [a]
set2List (Nilt) = []
set2List (Node a tl tr) = (set2List tl)++[a]++(set2List tr)

--retorna a cardinalidade de um conjunto
card :: Ord a => Set a -> Int
card s = length (set2List s)

--verifica se 2 conjuntos são iguais
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet a b = (set2List a) == (set2List b)

--realiza operação união entre 2 conjuntos
union :: Ord a => Set a -> Set a -> Set a
union a b = makeSet ((set2List a)++(set2List b))

--realiza operação interseção entre 2 conjuntos
inter :: Ord a => Set a -> Set a -> Set a
inter a b = makeSet (filter (\x -> memSet b x) (set2List a))

--reliza opereção diferença entre 2 conjuntos
diff :: Ord a => Set a -> Set a -> Set a
diff a b = makeSet (filter (\x -> not(memSet b x)) (set2List a))

--checa se um conjunto é subconjunto de outro conjunto
subSet :: Ord a => Set a -> Set a -> Bool
subSet a b = eqSet (union a b) b

mapSet :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet f s = makeSet (map f (set2List s))

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet p s = makeSet (filter p (set2List s))

foldSet :: Ord a => (a -> a -> a) -> a -> Set a -> a
foldSet f x s = (foldr f x (set2List s))