-- Jugador (el constructor no se exporta).
module Jugador
  ( Jugador
  , posJugador
  , velJugador
  , vidas
  , puntuacion
  , crearJugador
  , perderVida
  , sumarPuntos
  , conVelocidad
  , moverJugador
  , estadoSiguiente
  ) where

import Objeto (Objeto(..))
import Tipos (EstadoJuego(..), Vector2D, escalarVector, sumaVectores)

-- datos del jugador
data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

-- funcion que crea un jugador
crearJugador :: Vector2D -> Jugador
crearJugador p = Jugador { posJugador = p, velJugador = (0, 0), vidas = 3, puntuacion = 0 }

-- funcion que quita una vida (minimo 0)
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- funcion que suma puntos
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

-- funcion que cambia la velocidad
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j { velJugador = v }

-- funcion que mueve el jugador dependiente de su velocidad
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j { posJugador = sumaVectores (posJugador j) (escalarVector dt (velJugador j)) }

-- funcion que calcula el estado del juego
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDanio tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDanio = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando

-- instancia de jugador
instance Objeto Jugador where
  posicion = posJugador
  boundingBox j = (x - 30, y, 60, 90)
    where (x, y) = posJugador j
