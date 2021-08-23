module TNAgustinCampagnaTP1
where

{-
MG:
Corrección: Reentrega amistosa.
Mejorar el código en todos los ejercicios con las correcciones.
Reescribir el 4 y, por las dudas, no usar un contador.
-}

{- Recibe un número natural n y devuelve True si y solo si n es par, 
mayor que 2, y suma de dos numeros primos o False en caso contrario-}
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | esImpar n || n <= 2 = False
                    | otherwise           = descomponibleEnPrimos n (n-1)

{-
MG:
Ejercicio 1: R+
El primer caso no es undefined, es False.
Llamar a descomponibleEnPrimos está bien, pero esa función después tiene cosas para corregir internamente.
-}

{- Recibe un número natural n par mayor que 2 y devuelve True si y solo sí
la conjetura es cierta para todos los naturales pares mayores que 2 y menores o
iguales que n o False en caso contrario. -}
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | n == 4    = True
                          | otherwise = satisfaceGoldbach n && verificarConjeturaHasta (n-2)

{-
MG:
Ejercicio 2: B
Está bien pero se puede escribir de forma más declarativa en a lo sumo dos guardas.
-}

{-Recibe un número natural n par mayor que 2 y devuelve un par ordenado
(a,b) de números primos tales que a + b == n.-}
descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n | descomponibleEnPrimos n (n-1) = descomposicionEnPrimosCalc n (n-1)
                         | otherwise                     = undefined

{-
MG:
Ejercicio 3: R+
Mismas observaciones que en el ejercicio 1, la función descomposicionEnPrimosCalc debería salir más simple.
-}

{-Recibe un número natural n par mayor que 2 y devuelve la cantidad de pares
ordenados (a, b) de números primos tales que a + b == n.-}
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n | esImpar n || n <= 2 || not (descomponibleEnPrimos n (n-1)) = undefined
                           | otherwise                                                  = numeroDeDescomposicionesCalc n (n-1)

{-
MG:
Ejercicio 4: R+
Mismas observaciones que los anteriores, tiene que salir más simple y declarativo.
-}

--Funciones auxiliares

{-Recibe un numero natural n par mayor a dos, y un numero k, 
verifica si n puede formarse como la suma de dos numeros primos uno de ellos siendo menor o igual a k-}
descomponibleEnPrimos :: Integer -> Integer -> Bool
descomponibleEnPrimos n k | esPrimo k && esPrimo (n-k) = True
                          | k <= 1                     = False
                          | otherwise                  = descomponibleEnPrimos n (k-1)

{-
MG: 
La función descomponibleEnPrimos tiene muchisimas condiciones, muchas de ellas innecesarias, y se hace un poco difícil de seguir qué es lo que hace.
No es necesario recibir tantos parámetros ni chequear tantas condiciones. Tendría que salir de una forma tal que por ejemplo no haya que chequear si i+j == n, que ya sepamos que eso pasa.
Siempre es bueno sentarse y pensar casos a mano. Por ejemplo, de cuántas formas puedo escribir al 6 como suma de dos números naturales?
5+1
4+2
3+3
2+4
1+5
Entonces, tiene sentido por ejemplo pasar por el caso i=3, j=2?
Repensarlo para que salga de una forma más simple y declarativa.
-}

{-Recibe un numero natural n par mayor a dos, y un numero k, 
devuelve la descomposicion de n como la tupla de dos numeros primos cuya suma sea igual a n, siento uno de estos menor o igual a k-}
descomposicionEnPrimosCalc :: Integer -> Integer -> (Integer, Integer)
descomposicionEnPrimosCalc n k | esPrimo k && esPrimo (n-k) = (k,(n-k))
                               | otherwise                  = descomposicionEnPrimosCalc n (k-1)

{-
MG:
Mismas observaciones que aplican para descomponibleEnPrimos.
-}

{-Recibe un numero natural n par mayor a dos, y un numero k, 
devuelve la cantidad de descomposiciones posibles de n como la suma de dos numeros primos siendo uno de estos menor o igual a k-}
numeroDeDescomposicionesCalc :: Integer -> Integer -> Integer
numeroDeDescomposicionesCalc n k | k <= 1                     = 0
                                 | esPrimo k && esPrimo (n-k) = 1 + numeroDeDescomposicionesCalc n (k-1)
                                 | otherwise                  = numeroDeDescomposicionesCalc n (k-1)

{-
MG:
Nuevamente, tiene que salir más simple chequear que un número sea suma de dos primos y, en este caso, además ir sumando 1 cada vez que encontramos una combinación satisfactoria.
-}

--Recibe un numero natural n, si es impar devuelve True o False en caso contrario
esImpar :: Integer -> Bool
esImpar n = mod n 2 /= 0

--Recibe un numero natural n, si es par devuelve True o False en caso contrario
esPar :: Integer -> Bool
esPar n = mod n 2 == 0

--Recibe dos numeros natural n y m, si ambos son impares devuelve True, si ambos son pares tambien devuelve True, en otro caso devuelve False
mismaParidad :: Integer -> Integer -> Bool
mismaParidad n m = (esImpar n && esImpar m) || (esPar n && esPar m)

--Recibe un numero natural n y devuelve True si es primo o False en caso contrario
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = 1 == menorDivisor n

--Recibe un numero natural n y devuelve su menor divisor
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n (div n 2)

{-Recibe dos numeros naturales n y k, si k es divisor de n devuelve k, 
en caso contrario, itera bajando en 2 cuando son de igual paridad, o en 1 si no,
el numero de k, hasta encontrar el menor divisor de n-}
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | mismaParidad n k = menorDivisorDesde n (k-2)
                      | otherwise        = menorDivisorDesde n (k-1)