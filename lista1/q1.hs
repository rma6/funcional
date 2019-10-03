import Data.Char

let2int :: Char -> Int
int2let :: Int -> Char
shift :: Int -> Char -> Char
encode :: Int -> String -> String
percent :: Int -> Int-> Float
freqs :: String -> [Float]
chisqr :: [Float] -> [Float] -> Float
rotate :: Int -> [a] -> [a]
crack :: String -> String

--aux
countChar :: String -> [Int] -> [Int]
makeIntList :: Int -> [Int]
insertAt :: Int -> a -> [a] -> [a]
minIndex :: Ord a => Int -> [a] -> Int

let2int c = (ord c)-97

int2let i = chr (i+97)

shift _ ' ' = ' '
shift i c = int2let(mod ((let2int c)+i) 26)

encode i s = map (shift i) s

percent a b = 100*(fromIntegral a)/(fromIntegral b)

makeIntList 0 = []
makeIntList i = [0]++makeIntList (i-1)

insertAt p e (a:ax) | p == 0 = e:ax
                    | otherwise = a:(insertAt (p-1) e ax)

countChar [] l = l
countChar (' ':cx) l = countChar cx l
countChar (c:cx) l = countChar cx (insertAt (let2int c) ((l!!(let2int c))+1) l)

freqs s = map (`percent` (length s)) (map fromIntegral (countChar s (makeIntList 26)))

chisqr [] [] = 0.0
chisqr (a:ax) (b:bx) = ((a-b)**2/b)+(chisqr ax bx)

rotate 0 l = l
rotate n (a:ax) = rotate (n-1) (ax++[a])

minIndex i l | l!!i == (minimum l) = i
             | otherwise = minIndex (i+1) l

crack s = encode (26-(minIndex 0 [chisqr (rotate n (freqs s)) table | n <- [0..25]])) s
    where table = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074]