module Partida
    ( Partida(..)
    , Entrada(..)
    , serializarPartida
    , parsearPartida
    , parsearEntrada
    , parsearLeaderboard
    , mejoresPuntuaciones
    , parsearPartidaA
    , puntuacionTotal
    ) where


import Nivel

import qualified Data.List as L
import Data.Ord
import Text.Read

data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

-- Convierte una partida en una lista de líneas de texto.
serializarPartida :: Partida -> [String]
serializarPartida p =
    [ "NIVEL: " ++ show (nivelGuardado p),
    "VIDAS: " ++ show (vidasGuardadas p),
    "PUNTUACION: " ++ show (puntuacionGuardada p)]

-- Convierte una lista de líneas en una partida si todos los datos son válidos.
parsearPartida :: [String] -> Maybe Partida
parsearPartida xs = do
    nivel <- buscarClave "NIVEL: " xs
    vida <- buscarClave "VIDAS: " xs
    puntuacion <- buscarClave "PUNTUACION: " xs
    n <- readMaybe nivel
    v <- readMaybe vida
    p <- readMaybe puntuacion
    return (Partida n v p)

-- Convierte una línea en una entrada si tiene un nombre y una puntuación válidos.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea = case splitOn ';' linea of
                        [nombre, valor] | not (null nombre) -> do
                            p <- readMaybe valor
                            return (Entrada nombre p)
                        _ -> Nothing

-- Convierte las líneas no vacías en entradas, fallando si alguna es inválida.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard xs = mapM parsearEntrada (filter (not . null) xs)

-- Devuelve las n entradas con mayor puntuación.
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n = take n . L.sortOn (Down . puntos)

-- BONUS

-- Parsea una partida utilizando operaciones applicative.
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA xs =
    Partida
        <$> (buscarClave "NIVEL: " xs >>= readMaybe)
        <*> (buscarClave "VIDAS: " xs >>= readMaybe)
        <*> (buscarClave "PUNTUACION: " xs >>= readMaybe)

-- Suma los puntos de una tabla de puntuaciones válida.
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal xs = sum . map puntos <$> parsearLeaderboard xs