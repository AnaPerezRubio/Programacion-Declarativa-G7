-- =============================================================
-- PARTIDA.hs
-- =============================================================

module Partida (
    Partida(..), Entrada(..), serializarPartida, parsearPartida, parsearEntrada,
    parsearLeaderboard, mejoresPuntuaciones, parsearPartidaA, puntuacionTotal
) where

import Nivel (buscarClave, splitOn)
import Text.Read (readMaybe)
import qualified Data.List as List (sortOn)
import Data.Ord  

data Partida = Partida{ nivelGuardado :: Int, vidasGuardadas :: Int,
                        puntuacionGuardada :: Int}
  deriving (Eq, Show)

data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)
  
-- Convierte una partida en lista de Strings
serializarPartida :: Partida -> [String]
serializarPartida (Partida nivel vidas pts) = 
  ["NIVEL: " ++ show nivel,
   "VIDAS: " ++ show vidas,
   "PUNTUACION: " ++ show pts]

-- Convierte una lista de Strings en una Partida
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
  nivelLeido   <- buscarClave "NIVEL: " lineas
  vidasLeidas  <- buscarClave "VIDAS: " lineas
  puntosLeidos <- buscarClave "PUNTUACION: " lineas
  
  nivel <- readMaybe nivelLeido
  vidas <- readMaybe vidasLeidas
  pts   <- readMaybe puntosLeidos
  
  return (Partida nivel vidas pts)

-- Convierte una cadena en una entrada
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea = 
  case splitOn ';' linea of
    [nombre, puntosLeidos] -> do 
         puntos <- readMaybe puntosLeidos  
         return (Entrada nombre puntos)
    _ -> Nothing

-- Convierte una lista de cadenas en una lista de entradas, si hay error devuelve Nothing
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard = mapM parsearEntrada

-- Devuelve las n mejores puntuaciones dada una lsta de Entradas
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones _ [] = []
mejoresPuntuaciones n xs = take n $ List.sortOn (Down . puntos) xs

-- -------------------------------------------------------------
-- BONUS: 
-- -------------------------------------------------------------

-- parsearPartidaA: usando <$>, <*>, >>=
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas = Partida <$> nivel <*> vidas <*> pts
  where nivel = buscarClave "NIVEL: " lineas >>= readMaybe
        vidas = buscarClave "VIDAS: " lineas >>= readMaybe
        pts = buscarClave "PUNTUACION: " lineas >>= readMaybe

-- puntuacionTotal: usando fmap, <$>
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas = sum . map puntos <$> parsearLeaderboard lineas