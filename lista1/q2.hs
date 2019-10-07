import Prelude hiding ((&&), (||), not)
import qualified Prelude

data Prop = Const Bool |
            Var Char |
            Imply Prop Prop |
            And Prop Prop |
            Or Prop Prop |
            Xor Prop Prop |
            Not Prop
            deriving (Show)

not :: Prop -> Prop
not (Const a) = Const (Prelude.not a)

(&&) :: Prop -> Prop -> Prop
(&&) (Const a) (Const b) = Const ((Prelude.&&) a b)

(||) :: Prop -> Prop -> Prop
(||) (Const a) (Const b) = Const ((Prelude.||) a b)

type Subst = [(Char, Bool)]

eval :: Subst -> Prop -> Prop
vars :: Prop -> [Char]
bools :: Int -> [[Bool]]
substs :: Prop -> [Subst]
isTaut :: Prop -> Bool

--acha a tupla com a variável desejada
find :: Subst -> Char -> Bool
--remove duplicatas
rmDups :: String -> String
--vars com duplicatas
varsd :: Prop -> [Char]
--incrementa um número binário expresso como lista de booleanos
incrementBin :: [Bool] -> [Bool]
--cria lista de boolenos inicializados com false
makeBoolList :: Int -> [Bool]
--função auxiliar para gerar table verdade
makeTable :: [Bool] -> Int -> [[Bool]]
--cria um Subst a partir de uma lista de variáveis e uma lista de boolenaos contendo seus valores
makeSubst :: String -> [Bool] -> Subst
--cria uma lista de Subst
makeSubsts :: String -> [[Bool]] -> [Subst]

find ((c, b):ax) m | c==m = b
                   | otherwise = find ax m

rmDups [] = []
rmDups (a:ax) | elem a ax = rmDups ax
              | otherwise = [a]++(rmDups ax)

varsd (Const c) = []
varsd (Var v) = [v]
varsd (Imply a b) = (vars a)++(vars b)
varsd (And a b) = (vars a)++(vars b)
varsd (Or a b) = (vars a)++(vars b)
varsd (Xor a b) = (vars a)++(vars b)
varsd (Not a) = (vars a)

incrementBin [] = []
incrementBin (True:x) = False:(incrementBin x)
incrementBin (False:x) = True:x

makeBoolList 0 = []
makeBoolList i = [False]++makeBoolList (i-1)

makeTable _ 0 = []
makeTable bin i = [bin]++(makeTable (incrementBin bin) (i-1))

makeSubst [] [] = []
makeSubst (a:ax) (b:bx) = [(a, b)]++(makeSubst ax bx)

makeSubsts _ [] = []
makeSubsts v (b:bs) = (makeSubst v b):(makeSubsts v bs)

eval _ (Const c) = Const c
eval s (Var v) = Const (find s v)
eval s (Imply a b) = (not (eval s a)) || (eval s b)
eval s (And a b) = (eval s a) && (eval s b)
eval s (Or a b) = (eval s a) || (eval s b)
eval s (Xor a b) = ((not (eval s a)) && (eval s b)) || ((not (eval s b)) && (eval s a))
eval s (Not a) = not (eval s a)

vars p = rmDups (varsd p)

bools n = makeTable (makeBoolList n) (2^n)

substs p = makeSubsts (vars p) (bools (length (vars p)))

isTaut p = foldr (Prelude.&&) True (map (\(Const a) -> a) (map (`eval` p) (substs p)))