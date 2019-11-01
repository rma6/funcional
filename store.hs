type Var = Char
data Store = Store [(Integer,Var)]

initial :: Store
initial = Store []

value :: Store -> Var -> Maybe Integer
value (Store []) var = Nothing
value (Store ((i, v):ax)) var | v == var = Just i
                              | otherwise = value (Store ax) var

update :: Store -> Var -> Integer -> Store
update (Store []) var int = Store [(int, var)]
update (Store ((i, v):ax)) var int | v == var = Store ([(int, var)]++ax)
                                   | 