-- A função de união usa o fato de que as listas estão ordenadas

union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys)
    |  x<y = x : uni xs (y:ys)
    |  x==y = x : uni xs ys
    |  otherwise = y : uni (x:xs) ys

-- A função para interseção também age sobre listas ordenadas
inter :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys)
    |  x<y = int xs (y:ys)
    |  x==y = x : int xs ys
    |  otherwise = int (x:xs) ys

diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif (x:xs) (y:ys)
    |  x<y = x : dif xs (y:ys) |  x==y = dif xs ys
    |  otherwise = dif (x:xs) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys)
    |  x<y  = False
    |  x==y = subS xs ys
    |  x>y  = subS (x:xs) ys


-- Igualdade é definida com base na igualdade entre listas, o que é válido apenas porque elas não são arbitrárias
eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

--A ordem entre subconjuntos não é total, ao contrário da função <= da classe Ord (ou x ≤ ou y ≤ x é verdadeiro)
leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

-- Para formar um conjunto a partir de uma lista arbitrária, a lista é ordenada e, então, os elementos duplicados são removidos antes de serem estruturados como Set
makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
    where remDups [] = []
          remDups [x] = [x]
          remDups (x:y:xs) 
            | x < y = x : remDups (y:xs) |  otherwise = remDups ( y : xs )

--Possuem o mesmo comportamento que as funções de alta ordem para lista, mas agindo sobre conjuntos
mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set xs) = (foldr f x xs)
