module Clase03
where
        import Clase01

        factorial :: Int -> Int
        factorial n | n == 0 = 1
                    | n >  0 = n * factorial (n-1)
                    | n <  0 = undefined

        superFactorial :: Integer -> Integer
        superFactorial n | n == 0 = 1
                         | n >  0 = n * superFactorial (n-1)
                         | n <  0 = undefined

        pmFactorial :: Integer -> Integer
        pmFactorial 0 = 1
        pmFactorial n = n * pmFactorial (n-1)

        esPar' :: Int -> Bool
        esPar' n | n == 0 = True
                 | n == 1 = False
                 | otherwise = esPar (n-2)

        fib :: Int -> Int
        fib 0 = 0
        fib 1 = 1
        fib n = fib (n-2) + fib(n-1)

        parteEntera :: Float -> Integer
        parteEntera x | x < 0 = undefined
                | x < 1 = 0
                | otherwise = 1 + parteEntera (x-1)

        --Pactica

        {- Escribir una función para determinar si un número natural es múltiplo de 3. No está
        permitido utilizar mod ni div.-}
        multiploDeTres :: Int -> Bool
        multiploDeTres x | x < 3 = False
                        | x == 3 = True
                        | otherwise = multiploDeTres (x - 3)

        {- Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n
        números impares. -}
        sumaImpares :: Int -> Int
        sumaImpares x | x == 1 = 1
                      | otherwise = (x*2) - 1 + sumaImpares (x -1)

        --Escribir una función medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
        medioFact :: Int -> Int
        medioFact x | x <= 1 = 1
                    | otherwise = x * medioFact (x - 2)

        {- Escribir una función que determine la suma de dı́gitos de un número positivo. Para esta
        función pueden utilizar div y mod.-}
        sumaDigitos :: Int -> Int
        sumaDigitos x | x == 0 = 0
                      | otherwise = mod x 10 + sumaDigitos (div x 10)


        -- Implementar una función que determine si todos los dı́gitos de un número son iguales.
        igualesDigitos :: Int -> Bool
        igualesDigitos x | div x 10 == 0 = True
                         | unitDigit x == decenDigit x = igualesDigitos (div x 10)
                         | otherwise = False