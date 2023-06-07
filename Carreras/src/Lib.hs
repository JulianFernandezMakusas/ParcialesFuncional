module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-------------
-- Punto 1 --
-------------
data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show)

data Carrera = Carrera{
    autosParticipando :: [Auto]
} deriving (Show)

auto1 = Auto{color="Verde", velocidad=80, distancia=40}
auto2 = Auto{color="Azul", velocidad=80, distancia=25}
auto3 = Auto{color="Negro", velocidad=80, distancia=20}
carrera = Carrera{autosParticipando=[auto1,auto2,auto3]}
-------
-- a --
-------
estaCerca :: Auto -> Auto -> Bool                                
estaCerca unAuto otroAuto = (compararColores unAuto otroAuto) && ((<10) . abs . ((distancia otroAuto) -) . distancia $ unAuto)

compararColores :: Auto -> Auto -> Bool
compararColores unAuto otroAuto = color unAuto /= color otroAuto
-------
-- b --
-------
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (vaGanando auto carrera) &&  not(any (estaCerca auto) (autosParticipando carrera))

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto carrera = compararDistancias (distancia auto) (obtenerDistancias carrera) 

obtenerDistancias :: Carrera -> [Int]
obtenerDistancias carrera = map distancia (autosParticipando carrera)

compararDistancias :: Int -> [Int] -> Bool
compararDistancias distanciaAuto listaDistancias = distanciaAuto >= (maximum listaDistancias)

-------
-- c --
-------
puestoDelAuto :: Carrera -> Auto-> Int
puestoDelAuto carrera auto = (+1) . length . (filter (distancia auto <)) $ (obtenerDistancias carrera)

-------------
-- Punto 2 --
-------------

-------
-- a --
-------
corra :: Int -> Auto -> Auto
corra tiempo auto = Auto{color = color auto, velocidad = velocidad auto, distancia = (distancia auto) + tiempo*(velocidad auto)}

-------
-- b --
-------
type Modificador = Int -> Int

modificador :: Modificador -> Auto -> Auto
modificador modificador auto = Auto{color = color auto, velocidad = modificador . velocidad $ auto, distancia = distancia auto}

bajarVelocidad :: Int -> Modificador
bajarVelocidad relentizacion velocidad = velocidad - relentizacion

-------------
-- Punto 3 --
-------------
type PowerUp = Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista
-------
-- a --
-------
terremoto :: Auto -> PowerUp
terremoto auto carrera = Carrera{autosParticipando = afectarALosQueCumplen (estaCerca auto) (modificador (bajarVelocidad 50)) (autosParticipando carrera)}
-------
-- b --
-------
miguelitos :: Int -> Auto -> PowerUp
miguelitos velocidadABajar auto carrera = Carrera{autosParticipando = afectarALosQueCumplen (vaGanandoA carrera auto) (modificador (bajarVelocidad velocidadABajar)) (autosParticipando carrera)}

vaGanandoA :: Carrera -> Auto -> Auto -> Bool
vaGanandoA carrera unAuto otroAuto = puestoDelAuto carrera unAuto > puestoDelAuto carrera otroAuto
-------
-- c --
-------
jetpack :: Int -> Auto -> PowerUp
jetpack tiempo auto carrera = Carrera{autosParticipando = afectarALosQueCumplen (not . (compararColores auto)) (corra tiempo . modificador (*2)) (autosParticipando carrera)}

-------------
-- Punto 4 --
-------------
type Evento = Carrera -> Carrera
-------
-- a --
-------
simularCarrera :: Carrera -> [Evento] -> [(Int, String)]
simularCarrera carrera listaDeEventos = producirTablaDePosiciones . (aplicarEventos listaDeEventos) $ carrera

aplicarEventos :: [Evento] -> Carrera -> Carrera
aplicarEventos unosEventos carrera = foldl funcionAuxiliar carrera unosEventos

funcionAuxiliar :: Carrera -> Evento -> Carrera
funcionAuxiliar carrera unEvento = unEvento carrera

producirTablaDePosiciones :: Carrera -> [(Int, String)]
producirTablaDePosiciones carrera = ordenarTuplas . convertirListaAutoATuplas . autosParticipando $ carrera

convertirListaAutoATuplas :: [Auto] -> [(Int, String)]
convertirListaAutoATuplas listaAuto = map transformarATupla listaAuto

transformarATupla :: Auto -> (Int, String)
transformarATupla auto = (puestoDelAuto auto, color auto)

ordenarTuplas :: [(Int, String)] -> [(Int, String)]
ordenarTuplas [] = []
ordenarTuplas (x:xs) = ordenarTuplas (filter (<= x) xs) ++ x : ordenarTuplas (filter (>x) xs)
-------
-- b --
-------
correnTodos :: Int -> Evento
correnTodos tiempo carrera = Carrera{autosParticipando = (map corra tiempo) . autosParticipando $ carrera}

--usaPowerUp :: PowerUp -> Evento
--usaPowerUp powerUp carrera = Carrera{autosParticipando = }
-------
-- c --
-------

-------------
-- Punto 5 --
-------------
-------
-- a --
-------
-- La solucion lo permite ya que es posible saber el color de un auto
-------
-- b --
-------
-- No podria usarse la funcion va tranquilo ya que nunca terminaria de calcular las distancias para saber si va ganando.
-- Tampoco podria saber el puesto ya que se quedaria intentanto obtener las distancias de los autos que participan y como es una lista infinita no terminaria nunca