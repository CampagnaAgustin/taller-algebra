module Clase06
where

sumatoriaLista :: [Int] -> Int
sumatoriaLista l | l == [] = 0
                 | otherwise = head l + sumatoriaLista (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x== y = True
                   | pertenece x ys

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l | mod (head l) 45345 == 0 = head l
                        | otherwise = primerMultiploDe45345 (tail l)

sumatoriaListaPM :: [Int] -> Int
sumatoriaListaPM [] = 0
sumatoriaListaPM (x:xs) = x + sumatoriaListaPM xs