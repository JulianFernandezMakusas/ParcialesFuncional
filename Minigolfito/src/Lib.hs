module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
--undefined

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-------------
-- Punto 1 --
-------------
type Palo = Habilidad -> Tiro

---------
-- A I --
---------
putter :: Palo
putter habilidad = Tiro{velocidad = 10, precision = (*2) . precisionJugador $ habilidad, altura = 0}

----------
-- A II --
----------
madera :: Palo
madera habilidad = Tiro{velocidad = 100, precision = div (precisionJugador habilidad) 2, altura = 5}

-----------
-- A III --
-----------
hierro :: Int -> Palo
hierro n habilidad = Tiro{velocidad = (*n) . fuerzaJugador $ habilidad, precision = div (precisionJugador habilidad) n, altura = max 0 (n - 3)}

-------
-- B --
-------
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-------------
-- Punto 2 --
-------------
golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo . habilidad $ unJugador

-------------
-- Punto 3 --
-------------
data Obstaculo = Obstaculo{
    puedeSuperar :: (Tiro -> Bool),
    efectoObstaculo :: (Tiro -> Tiro)
} 

tiroFallido = Tiro{velocidad = 0, precision = 0, altura = 0}
tiroTunel = Tiro{velocidad = 1000, precision = 1000, altura = 0}
tiroLaguna = Tiro{velocidad = 91, precision = 85, altura = 2}
tiroHoyo = Tiro{velocidad = 10, precision = 96, altura = 0}

esMayorQue :: Int -> Int -> Bool
esMayorQue unValor otroValor = otroValor > unValor

alRasDelSuelo :: Int -> Bool
alRasDelSuelo altura = altura == 0

entre :: Int -> Int -> Int -> Bool
entre rango1 rango2 valor = (valor >= rango1) && (valor <= rango2)

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo unObstaculo unTiro
    | puedeSuperar unObstaculo unTiro = efectoObstaculo unObstaculo unTiro
    | otherwise                       = tiroFallido

-------
-- A --
-------
tunel :: Obstaculo
tunel = Obstaculo{puedeSuperar = superaTunel, efectoObstaculo = efectoTunel}

superaTunel :: Tiro -> Bool
superaTunel unTiro = ((esMayorQue 90) . precision $ unTiro) && (alRasDelSuelo . altura $ unTiro)

efectoTunel :: Tiro -> Tiro
efectoTunel unTiro = Tiro{velocidad = (*2) . velocidad $ unTiro, precision = 100, altura = 0}
-------
-- B --
-------
laguna :: Int -> Obstaculo
laguna largoDeLaguna = Obstaculo {puedeSuperar = superaLaguna, efectoObstaculo = (efectoLaguna largoDeLaguna)}

superaLaguna :: Tiro -> Bool
superaLaguna unTiro = ((esMayorQue 80) . velocidad $ unTiro) && ((entre 1 5) . altura $ unTiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoDeLaguna unTiro = Tiro{velocidad = velocidad unTiro, precision = precision unTiro, altura = div (altura unTiro) largoDeLaguna}
-------
-- C --
-------
hoyo :: Obstaculo
hoyo = Obstaculo {puedeSuperar = aciertaEnHoyo, efectoObstaculo = efectoHoyo}

aciertaEnHoyo :: Tiro -> Bool
aciertaEnHoyo unTiro = ((entre 5 20) . velocidad $ unTiro) && (alRasDelSuelo . altura $ unTiro) && ((esMayorQue 95) . precision $ unTiro)

efectoHoyo :: Tiro -> Tiro
efectoHoyo unTiro = Tiro{velocidad = 0, precision = 0, altura = 0}
-------------
-- Punto 4 --
-------------
-------
-- A --
-------
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (supero unJugador unObstaculo) palos

supero :: Jugador -> Obstaculo -> Palo -> Bool
supero unJugador unObstaculo unPalo = (puedeSuperar unObstaculo) . (golpe unJugador) $ unPalo

-------
-- B --
-------
--cuantosObstaculosSupera :: [Obstaculo] -> Tiro -> Int
--cuantosObstaculosSupera listaDeObstaculos unTiro = length . (takeWhile (puedeSuperar)) $ listaDeObstaculos

cuantosObstaculosSupera :: [Obstaculo] -> Tiro -> Int
cuantosObstaculosSupera [] unTiro = 0
cuantosObstaculosSupera (obstaculo:obstaculos) unTiro 
    | puedeSuperar obstaculo unTiro = 1 + ((cuantosObstaculosSupera obstaculos) . (efectoObstaculo obstaculo) $ unTiro)
    | otherwise                     = 0

listaDeObstaculos1 = [tunel, tunel, tunel, tunel, tunel, tunel, hoyo]

-------
-- C --
-------
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador listaDeObstaculos = maximoSegun ((cuantosObstaculosSupera listaDeObstaculos) . golpe unJugador) palos

-------------
-- Punto 5 --
-------------
jugadorDeTorneo = fst
puntosGanador = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = map (padre . jugadorDeTorneo) . filter (not . (gano puntosDeTorneo)) $ puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeJugador = all ((< puntosGanador puntosDeJugador) . puntosGanador) . filter (/= puntosDeJugador) $ puntosDeTorneo