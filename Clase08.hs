module Clase08
where
    import Clase04

    type Set a = [a]

    vacio :: Set a
    vacio = []

    combinatorio :: Int -> Int -> Int
    combinatorio n k = (fact n) `div`((fact k) * (fact(n-k)))

    combinatorio' :: Int -> Int -> Int
    combinatorio' n 0 = 1
    combinatorio' n k | n == k = 1
                      | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

    agregar :: Eq b => b -> Set b -> Set b
    agregar n c | n `elem` c = c 
                | otherwise = n:c 

    union :: Eq a => Set a -> Set a -> Set a
    union [] ys     = ys
    union (x:xs) ys = union xs (agregar x ys)

    agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
    agregarElementoAdelante x [] = []
    agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

    agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
    agregarElementosAListas [] _ = []
    agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosAListas xs c)

    variaciones :: Set Int -> Int -> Set [Int]
    variaciones c 0 = [[]]
    variaciones c k = agregarElementosAListas c (variaciones c (k-1))

    insertarEn :: [Int] -> Int -> Int -> [Int]
    insertarEn xs n 1     = n:xs
    insertarEn (x:xs) n i = x:insertarEn xs n (i-1)

    insertarEnCadaPosicion :: [Int] -> Int -> Int -> Set [Int]
    insertarEnCadaPosicion xs c 1 = agregar (insertarEn xs c 1) vacio
    insertarEnCadaPosicion xs c i = agregar (insertarEn xs c i) (insertarEnCadaPosicion xs c (i-1))

    insertarEnCadaPosicionDeTodasLasListas :: Set [Int] ->  Int -> Set [Int]
    insertarEnCadaPosicionDeTodasLasListas [] c = []
    insertarEnCadaPosicionDeTodasLasListas (xs:xss) c = (insertarEnCadaPosicion xs c (length xs +1)) `union` 
                                                        (insertarEnCadaPosicionDeTodasLasListas xss c)

    permutaciones :: Set Int -> Set [Int]
    permutaciones [] = [[]]
    permutaciones (c:cs) = insertarEnCadaPosicionDeTodasLasListas (permutaciones cs) c

    