module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Persona = Persona{
  nombre :: String,
  edad :: Int
} deriving (Show)

julian = Persona{nombre = "Julian", edad = 23}

cumpleAnios :: Persona -> Persona
cumpleAnios unaPersona = unaPersona {edad = edad unaPersona + 1}

-----------------------------------------------------------------

-------------
-- Punto 1 --
-------------

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
