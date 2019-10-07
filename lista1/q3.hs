sqroot :: Double -> Double

sqroot n = (iterate (\a -> (a+n/a)/2) 1.0)!!(length (takeWhile (>0.00001) [abs (((iterate (\a -> (a+n/a)/2) 1.0)!!(k+1))-((iterate (\a -> (a+n/a)/2) 1.0)!!k)) | k <- [0..]])+1)
--(1)iterate (\a -> (a+n/a)/2) 1.0) gera uma lista infinita com as aproximações da raiz quadrada de n geradas pelo método de newton
--(2)logo, queremos pegar a valor no índice no qual a diferença desse valor para o anterior seja menor que 0.00001
--(3)Para achar esse índice utilíza-se de compreensão de listas para gerar uma lista infinita com as diferenças entre o elemento na posição k+1 e k de iterate (\a -> (a+n/a)/2) 1.0)
--(4)Pegamos os valores dessa lista de diferenças até que apareça um valor menor ou igual 0.00001, fazemos isso utilizando a função takeWhile
--(5)o índice desejado em (2) é igual ao comprimento da lista gerada em (4) somado de 1