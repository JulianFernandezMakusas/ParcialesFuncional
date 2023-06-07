module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-----------------------------------------------------------------
-------------
-- Punto 1 --
-------------
data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show)

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}deriving (Show)

data Gema = Gema {

}deriving (Show)

type Universo = [Personaje]

chasquido :: Guantelete -> Universo -> Universo
chasquido unGuante unUniverso 
    | puedeUsarse unGuante = reducirMitad unUniverso
    | otherwise            = unUniverso

reducirMitad :: Universo -> Universo
reducirMitad unUniverso = take (div (length Universo) 2) unUniverso

puedeUsarse :: Guantelete -> Bool
puedeUsarse unGuante = ((==6) . length . gemas $ guantelete) && ((=="uru") . material $ guantelete)
-------------
-- Punto 2 --
-------------

-------------
-- Punto 3 --
-------------

-------------
-- Punto 4 --
-------------

-------------
-- Punto 5 --
-------------

-------------
-- Punto 6 --
-------------

-------------
-- Punto 7 --
-------------