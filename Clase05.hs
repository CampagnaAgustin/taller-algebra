fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

prod :: Int -> Int -> Int
prod d h | d == h    = d
prod d h | otherwise = h * prod d (h-1)

prod' :: Int -> Int -> Int
prod' d h | d == h    = d
prod' d h | otherwise = d * prod' (d+1) h

fact' :: Int -> Int
fact' n = prod 1 n

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1         = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise      = sumaDivisoresHasta n (k-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n         = n
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k+1)
                       | otherwise      = sumaDivisoresHasta n (k+1)

sumaDivisores' :: Int -> Int
sumaDivisores' n = sumaDivisoresDesde n 1

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n (n-1)

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise      = menorDivisorDesde n (k-1)
                    
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = 1 == menorDivisor n

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n - 1)