-- Lectura de partidas guardadas y tablas de puntuaciones (Maybe)
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

import Data.List (sortOn)
import Data.Ord (Down(..))
import Nivel (buscarClave, splitOn)
import Text.Read (readMaybe)

-- Partida guardada: nivel, vidas y puntuación
data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

-- Línea de la tabla de puntuaciones
data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

-- Busca una clave y lee su valor; falla si falta o no se puede leer
leerCampo :: Read a => String -> [String] -> Maybe a
leerCampo clave lineas = buscarClave clave lineas >>= readMaybe

-- Convierte una partida en las líneas del fichero de guardado
serializarPartida :: Partida -> [String]
serializarPartida (Partida niv vid pts) =
  [ "NIVEL: " ++ show niv
  , "VIDAS: " ++ show vid
  , "PUNTUACION: " ++ show pts
  ]

-- Interpreta una partida guardada (notación do); Nothing si falta algo o es ilegible
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
  niv <- leerCampo "NIVEL: " lineas
  vid <- leerCampo "VIDAS: " lineas
  pts <- leerCampo "PUNTUACION: " lineas
  return (Partida niv vid pts)

-- Interpreta una línea "nombre;puntos" con nombre no vacío y exactamente dos campos
parsearEntrada :: String -> Maybe Entrada
parsearEntrada texto = case splitOn ';' texto of
  [nombre, pts] | not (null nombre) -> Entrada nombre <$> readMaybe pts
  _                                 -> Nothing

-- Interpreta una tabla de puntuaciones ignorando líneas vacías; Nothing si alguna otra es inválida
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearEntrada . filter (not . null)

-- Las n mejores entradas, de mayor a menor puntuación
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n = take n . sortOn (Down . puntos)

-- Bonus: igual que parsearPartida pero con estilo Applicative (<$> y <*>)
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
  Partida <$> leerCampo "NIVEL: " lineas
          <*> leerCampo "VIDAS: " lineas
          <*> leerCampo "PUNTUACION: " lineas

-- Bonus: suma de los puntos de una tabla válida, aplicando la suma dentro del Maybe (Functor)
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas = sum . map puntos <$> parsearLeaderboard lineas
