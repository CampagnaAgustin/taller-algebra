module Clase02
where

estanRealacionados :: Float -> Float -> Bool
{-dados dos números reales, decide si están relacionados considerando
la relación de equivalencia en R cuyas clases de equivalencia son:
(−∞, 3], (3, 7] y (7, ∞).-}
estanRealacionados x y | x <= 3 = y <= 3
                       | x > 3 && x <= 7 = y > 3 && y <= 7
                       | x > 7 = y > 7

--calcula el producto interno entre dos vectores de R2.
prodInt :: (Num t) => (t, t) -> (t, t) -> t
prodInt (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

{-dados dos vectores de R2 , decide si es cierto que cada coordenada del primer
vector es menor a la coordenada correspondiente del segundo vector.-}
todoMenor :: (Ord t) => (t, t) -> (t, t) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

--calcula la distancia entre dos puntos de R2.
distanciaPuntos :: (Floating t) => (t, t) -> (t, t) -> t
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

--dada una terna de enteros, calcula la suma de sus tres elementos.
sumaTerna :: (Integral n) => (n, n, n) -> n
sumaTerna (x, y, z) = x + y + z

{-dada una terna de enteros, devuelve la posición del primer número par si
es que hay alguno, y devuelve 4 si son todos impares.-}
posicPrimerPar :: (Integral n) => (n, n, n) -> n
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise    = 4

{-crea un par a partir de sus dos componentes dadas por
separado (debe funcionar para elementos de cualquier tipo).-}
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

{-invierte los elementos del par pasado como parámetro
(debe funcionar para elementos de cualquier tipo).-}
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)