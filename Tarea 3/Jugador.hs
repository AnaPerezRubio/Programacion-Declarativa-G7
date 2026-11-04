-- =============================================================
-- JUGADOR.hs
-- =============================================================

module Jugador (
    Jugador, posJugador, velJugador, vidas, puntuacion, 
    crearJugador, perderVida, sumarPuntos, conVelocidad,
    moverJugador, estadoSiguiente
) where

import Tipos (Vector2D, sumaVectores, escalarVector, EstadoJuego(..))
import Objeto (Objeto(..))

data Jugador = Jugador {posJugador :: Vector2D, velJugador :: Vector2D,
                        vidas :: Int, puntuacion :: Int}
    deriving (Show, Eq)

instance Objeto Jugador where
  posicion = posJugador
  boundingBox j = (x-30, y, 60, 90)
    where (x,y) = posJugador j

-- Crea un jugador en la posicion dada, con velocidad nula, 3 vidas y 0 puntos
crearJugador :: Vector2D -> Jugador
crearJugador p = Jugador {posJugador = p, velJugador = (0,0),
                         vidas = 3, puntuacion = 0}

-- Resta una vida
perderVida :: Jugador -> Jugador
perderVida j = j {vidas = max 0 (vidas j - 1)}

-- Suma un determinado numero de puntos
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos p j = j {puntuacion = puntuacion j + p}

-- Actualiza la velocidad
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j {velJugador = v}

-- Dado un escalar, actualiza la posicion del jugador de acuerdo a 
-- la fórmula p + dt * v
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j {posJugador = nuevaPos}
  where 
   v = velJugador j
   p = posJugador j
   nuevaPos = sumaVectores p (escalarVector dt v)

-- Calcula el siguiente EstadoJuego
estadoSiguiente :: Jugador -> Bool -> Bool  -> EstadoJuego
estadoSiguiente j danyo meta
      | vidas j <= 0 = Gameover
      | danyo        = PierdeVida
      | meta         = NivelCompletado
      | otherwise    = Jugando
