module Clase07
where

type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x== y = True
                   | pertenece x ys

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise     = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise         = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
pertenece xs [] = False
pertenece xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = union (partes xs) (agregarATodos x (partes xs))