module TNAgustinCampagnaTP2
where

type Set a = [a]
type Posicion = [Int]
type Jugada = (Int, Int)

{-recibe una posición p, una jugada válida j y 
devuelve la posición obtenida alrealizar dicha jugada-}
jugar :: Posicion -> Jugada -> Posicion
jugar [] _ = []
jugar (p:ps) (j,k) | j == 1    = realizarJugada (p:ps) k
                   | otherwise = p:(jugar ps (j-1,k))

{-recibe una posición p y devuelve 
el conjunto de jugadas válidas a partir de p-}
posiblesJugadas :: Posicion -> Set Jugada
posiblesJugadas [] = []
posiblesJugadas p  = posiblesJugadasMayoresQueLaRecibida p (1,1)

--decide si una posición p es ganadora
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora [] = False
esPosicionGanadora p  = not (todasSonPosicionesGanadoras (jugarTodasLasJugadasPosibles p))

{-recibe una posición ganadora p y devuelve una jugada 
que dejaría al rival en una posición no ganadora.-}
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = jugadaGanadoraEntreLasRecibidas p (posiblesJugadas p)

{-recibe una posición p (no necesariamente ganadora) y 
devuelve la cantidad de jugadas ganadoras partiendo de p-}
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = numeroDeJugadasGanadorasEntreLasRecibidas p (posiblesJugadas p)

-- Funciones auxiliares

{-Recibe una posicion y el numero k de piedras a quitar, 
le quita k piedras al primer elemento de posicion,
y devuelve la posicion con las piedras quitadas-}
realizarJugada :: Posicion -> Int -> Posicion
realizarJugada (p:ps) k | p == k = ps
                        | p >= k = (p-k):ps

{-Recibe una posicion p, y una jugada valida (j,k),
devuelve una lista de todas las posibles jugadas (j',k') con j >= j' y k >= k' -}
posiblesJugadasMayoresQueLaRecibida :: Posicion -> Jugada -> Set Jugada
posiblesJugadasMayoresQueLaRecibida [] _ = []
posiblesJugadasMayoresQueLaRecibida (p:ps) (j,k) | p == k = (j,k):(posiblesJugadasMayoresQueLaRecibida ps ((j+1),1))
                                                 | otherwise = (j,k):(posiblesJugadasMayoresQueLaRecibida (p:ps) (j,(k+1)))

--Recibe una posicion p, y analiza si todas las posiciones p son ganadoras
todasSonPosicionesGanadoras :: Set Posicion -> Bool
todasSonPosicionesGanadoras (ps:pss) | pss == []  = esPosicionGanadora ps
                                     | otherwise  = (esPosicionGanadora ps) && (todasSonPosicionesGanadoras pss)

{-Recibe una posicion p, realiza todas las jugadas posibles sobre p 
y devuelve todas las posiciones obtenidas luego de jugar-}
jugarTodasLasJugadasPosibles :: Posicion -> Set Posicion
jugarTodasLasJugadasPosibles p = jugarLasJugadasRecibidas p (posiblesJugadas p)

{-Recibe una posicion p y un conjunto de jugadas validas j, 
realiza cada una de esas jugadas sobre p 
y devuelve la posicion final obtenida-}
jugarLasJugadasRecibidas :: Posicion -> Set Jugada -> Set Posicion
jugarLasJugadasRecibidas p [] = []
jugarLasJugadasRecibidas p (j:js) = (jugar p j):(jugarLasJugadasRecibidas p js)

{-Recibe una posicion p y un conjunto de jugadas validas j, 
busca cual es una jugada ganadora sobre p en el conjunto j y devuelve esa jugada-}
jugadaGanadoraEntreLasRecibidas :: Posicion -> Set Jugada -> Jugada
jugadaGanadoraEntreLasRecibidas p (j:js) | not (esPosicionGanadora (jugar p j)) = j
                                         | otherwise = jugadaGanadoraEntreLasRecibidas p js

{-Recibe una posicion p y un conjunto de jugadas validas j, 
calcula cuantas jugadas del conjunto j son ganadoras sobre p-}
numeroDeJugadasGanadorasEntreLasRecibidas :: Posicion -> Set Jugada -> Int
numeroDeJugadasGanadorasEntreLasRecibidas _ [] = 0
numeroDeJugadasGanadorasEntreLasRecibidas p (j:js) | not (esPosicionGanadora (jugar p j)) = 1 + numeroDeJugadasGanadorasEntreLasRecibidas p js
                                                   | otherwise = numeroDeJugadasGanadorasEntreLasRecibidas p js