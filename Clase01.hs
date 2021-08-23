module Clase01
where

g x y z = x + y + z*z

doble x = 2*x

suma x y = x + y

normaVectorial x1 x2 = sqrt(x1^2 + x2^2)

funcionConstante8 x = 8

f 0 = 1
f n = 0

signo 0 = 0
signo n | n > 0 = 1
        | otherwise = -1

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

maximoRac :: Float -> Float -> Float
maximoRac x y | x >= y = x
              | otherwise = y

f1 n | n >= 3 = 5

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 2
                         | otherwise = 0
                         where d = b^2 - 4*c

esMayorA9 :: Int -> Bool
esMayorA9 n | n > 9 = True
            | otherwise = False

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar2 n)

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y

--Tarea de la clase 1

--calcula el valor absoluto de un nuumero entero.
absolute :: Int -> Int
absolute x | x >= 0 = x
           | otherwise = -x

--devuelve el maximo entre el valor absoluto de dos numeros enteros.
absoluteMax :: Int -> Int -> Int
absoluteMax x y = maximo (absolute x) (absolute y)

--devuelve el maximo entre tres numeros enteros.
max3 :: Int -> Int -> Int -> Int
max3 x y z = maximo(maximo x y) z

--dados dos numeros racionales, decide si alguno de los dos es igual a 0
oneIs0 :: Float -> Float -> Bool
oneIs0 x y | x == 0 = True
           | y == 0 = True
           | otherwise = False

--dados dos numeros racionales, decide si alguno de los dos es igual a 0
pmOneIs0 :: Float -> Float -> Bool
pmOneIs0 0 _ = True
pmOneIs0 _ 0 = True
pmOneIs0 _ _ = False

--dados dos numeros racionales, decide si ambos son iguales a 0
bothAre0 :: Float -> Float -> Bool
bothAre0 x y | x == 0 && y == 0 = True
             | otherwise = False

--dados dos numeros racionales, decide si ambos son iguales a 0
mpBothAre0 :: Float -> Float -> Bool
mpBothAre0 0 0 = True
mpBothAre0 _ _ = False

--dados dos numeros naturales, decidir si el primero es multiplo del segundo.
isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y | mod y x == 0 = True
                 | otherwise = False

--dado un numero natural, extrae su digito de las unidades.
unitDigit :: Int -> Int
unitDigit x = mod x 10

--dado un numero natural, extrae su digito de las decenas.
decenDigit :: Int -> Int
decenDigit x = div (mod x 100) 10