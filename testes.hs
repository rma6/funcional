--tesp :: String -> String -> Int
--tesp d w = minimum [x | l <- w, x <- occ d l]

--remove character from string
rcs :: String -> Char -> (String, Bool)
rcs [] _ = [],
rcs (x:xs) c | x == c = xs
             | otherwise = x:(rcs xs c)

--make word
makew :: String -> String -> String


btoi :: String -> Int
btoi s = ibtoi (reverse s) 1

ibtoi :: String -> Int -> Int
ibtoi [] _ = 0
ibtoi ('0':xs) n = (ibtoi xs (n*2))
ibtoi ('1':xs) n = n+(ibtoi xs (n*2))