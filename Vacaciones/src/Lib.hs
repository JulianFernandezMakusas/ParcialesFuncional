module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-----------------------------------------------------------------
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

-------------
-- Punto 1 --
-------------

data Idioma = Español | Aleman | Catalan | Melmacquiano deriving (Show)

data Turista = Turista {
    nivelDeCansancio :: Int,
    nivelDeStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}deriving (Show)
-------
-- A --
-------
ana :: Turista
ana   = Turista{nivelDeCansancio = 0 , nivelDeStress = 21, viajaSolo = False, idiomas = [Español]}
-------
-- B --
-------
beto :: Turista
beto  = Turista{nivelDeCansancio = 15, nivelDeStress = 15, viajaSolo = True , idiomas = [Aleman]}

cathi :: Turista
cathi = Turista{nivelDeCansancio = 15, nivelDeStress = 15, viajaSolo = True , idiomas = [Aleman, Catalan]}
-------------
-- Punto 2 --
-------------
data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)

type Excursion = Turista -> Turista

cambiarCansansio :: Int -> Turista -> Turista
cambiarCansansio unValor unTurista = unTurista{nivelDeCansancio = (nivelDeCansancio unTurista) + unValor}

cambiarStress :: Int -> Turista -> Turista
cambiarStress unValor unTurista = unTurista{nivelDeStress = (nivelDeStress unTurista + unValor)}

cambiaAAcompaniado :: Turista -> Turista
cambiaAAcompaniado unTurista = unTurista{viajaSolo = False}

agregarIdioma :: Idioma -> Turista -> Turista
agregarIdioma unIdioma unTurista = unTurista{idiomas = (idiomas unTurista) ++ [unIdioma]}
---------------------------------------------------------------------------------
irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista = cambiarCansansio (-5) unTurista
    | otherwise           = cambiarStress    (-1) unTurista

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje unElemento unTurista = cambiarStress (-length unElemento) unTurista

salirAHablarIdioma :: Idioma -> Excursion
salirAHablarIdioma unIdioma unTurista = (agregarIdioma unIdioma) . cambiaAAcompaniado $ unTurista

caminar :: Int -> Excursion
caminar unaCantidadDeMinutos unTurista = (cambiarCansansio (calcularIntensidad unaCantidadDeMinutos)) . (cambiarStress (-calcularIntensidad unaCantidadDeMinutos)) $ unTurista

calcularIntensidad :: Int -> Int
calcularIntensidad unaCantidadDeMinutos = div unaCantidadDeMinutos 4

paseoEnBarco :: Marea -> Excursion
paseoEnBarco estadoMarea unTurista
    | estaFuerte estadoMarea    = (cambiarCansansio 10) . (cambiarStress 6) $ unTurista
    | estaModerada estadoMarea  = unTurista
    | otherwise                 = (salirAHablarIdioma Aleman) . (apreciarElementoDelPaisaje "mar") . (caminar 10) $ unTurista

estaFuerte :: Marea -> Bool
estaFuerte unaMarea = unaMarea == Fuerte

estaModerada :: Marea -> Bool
estaModerada unaMarea = unaMarea == Moderada
-------
-- A --
-------
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = (cambiarStress (nivelDeStress unTurista * (div 10 100))) . unaExcursion $ unTurista
-------
-- B --
-------
type Indice = Turista -> Int
deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun unIndice unTurista unaExcursion  = deltaSegun unIndice (hacerExcursion unaExcursion unTurista) unTurista
-------
-- C --
-------
-------
-- I --
-------
esEducativa :: Excursion -> Turista -> Bool
esEducativa unaExcursion unTurista = (>0) . (deltaExcursionSegun aprendioIdioma unTurista) $ unaExcursion

aprendioIdioma :: Turista -> Int
aprendioIdioma unTurista = length . idiomas $ unTurista
--------
-- II --
--------
excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes listaExcursiones unTurista = filter (esDesestresante unTurista) listaExcursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = (>3) . (deltaExcursionSegun nivelDeStress unTurista) $ unaExcursion
-------------
-- Punto 3 --
-------------
type Tour = [Excursion]

completo :: Tour
completo = [(caminar 20), (apreciarElementoDelPaisaje "cascada"), (caminar 40), (irALaPlaya), (salirAHablarIdioma Melmacquiano)]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [(paseoEnBarco Tranquila)] ++ [(unaExcursion)] ++ [(caminar 120)]

islaVecina :: Marea -> Tour
islaVecina unaMarea 
    | estaFuerte unaMarea = [(paseoEnBarco unaMarea), (apreciarElementoDelPaisaje "lago"), (paseoEnBarco unaMarea)]
    | otherwise           = [(paseoEnBarco unaMarea), (irALaPlaya), (paseoEnBarco unaMarea)]
-------
-- A --
-------
hacerTour :: Tour -> Turista -> Turista
hacerTour unTour unTurista = foldr (hacerExcursion) (cambiarStress (length unTour) unTurista) unTour
-------
-- B --
-------
tourConvincentePara :: Turista -> [Tour] -> [Tour]
tourConvincentePara unTurista listaDeTour = filter (tourConvincente unTurista) listaDeTour

tourConvincente :: Turista -> Tour -> Bool
tourConvincente unTurista unTour = any (dejaAcompaniado unTurista) . ((flip excursionesDesestresantes) unTurista) $ unTour 

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado unTurista unaExcursion = not . viajaSolo . (hacerExcursion unaExcursion) $ unTurista
-------
-- C --
-------
efectividadDeTour :: [Turista] -> Tour -> Int
efectividadDeTour listaDeTuristas unTour = sum . map (espiritualidadAportada unTour) . filter ((flip tourConvincente) unTour) $ listaDeTuristas

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada unTour listaDeTuristas = negate . (deltaRutina unTour) $ listaDeTuristas

deltaRutina :: Tour -> Turista -> Int
deltaRutina unTour unTurista = deltaSegun nivelDeRutina (hacerTour unTour unTurista) unTurista

nivelDeRutina :: Indice
nivelDeRutina turista = nivelDeCansancio turista + nivelDeStress turista
-------------
-- Punto 4 --
-------------

-------
-- A --
-------

-------
-- B --
-------

-------
-- C --
-------