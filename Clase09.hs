module Clase08
where

divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a-d) 'divNat' d + 1

modNat :: Int -> Int -> Int
modNat a d = a - d*(a 'divNat' d)

modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise = abs d - r'
        where r' = abs a 'modNat' abs d

dividido :: Int -> Int -> Int
dividido a d = sgq * absq
    where absq = abs (a-r) 'divNat (abs d)
          sgq = (signum a) * signum d)
          r = a 'modulo' d

