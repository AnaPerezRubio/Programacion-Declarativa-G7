-- partida guardada y puntuaciones
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

-- import cualificado de Data.List
import qualified Data.List as L
import Data.Ord (Down(..))
import Text.Read (readMaybe)

import Nivel (buscarClave, splitOn)

-- data de partida guardada
data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

-- data de linea de la tabla de puntuaciones
data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

-- funcion que pasa una partida a lineas de texto
serializarPartida :: Partida -> [String]
serializarPartida (Partida n v p) =
  [ "NIVEL: " ++ show n
  , "VIDAS: " ++ show v
  , "PUNTUACION: " ++ show p
  ]

-- funcion que lee una partida
parsearPartida :: [String] -> Maybe Partida
parsearPartida ls = do
  n <- buscarClave "NIVEL: " ls >>= readMaybe
  v <- buscarClave "VIDAS: " ls >>= readMaybe
  p <- buscarClave "PUNTUACION: " ls >>= readMaybe
  return (Partida n v p)

-- funcion que una linea de puntuacion
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea = case splitOn ';' linea of
  [nombre, pts] | not (null nombre) -> Entrada nombre <$> readMaybe pts
  _                                 -> Nothing

-- funcion que lee la tabla de puntuaciones
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearEntrada . filter (not . null)

-- funcion que devuelve las n mejores puntuaciones
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n = take n . L.sortOn (Down . puntos)

-- parsearPartida con Applicative
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA ls =
  Partida <$> campo "NIVEL: " <*> campo "VIDAS: " <*> campo "PUNTUACION: "
  where
    campo :: String -> Maybe Int
    campo clave = buscarClave clave ls >>= readMaybe

-- suma de todos los puntos
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal ls = sum . map puntos <$> parsearLeaderboard ls
