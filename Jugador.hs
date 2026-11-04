-- Jugador como tipo abstracto: el constructor no se exporta, así que solo crearJugador lo construye
module Jugador
  ( Jugador            -- exportación opaca: sin constructor
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

-- Jugador con posición, velocidad, vidas restantes y puntuación
data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

instance Objeto Jugador where
  posicion = posJugador
  boundingBox j = (x - 30, y, 60, 90)
    where (x, y) = posJugador j

-- Crea un jugador en la posición dada con velocidad nula, 3 vidas y 0 puntos
crearJugador :: Vector2D -> Jugador
crearJugador p = Jugador p (0, 0) 3 0

-- Quita una vida al jugador sin bajar nunca de 0
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Suma puntos a la puntuación del jugador
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

-- Fija la velocidad del jugador
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j { velJugador = v }

-- Mueve al jugador: posición + dt · velocidad
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j { posJugador = sumaVectores (posJugador j) (escalarVector dt (velJugador j)) }

-- Calcula el estado de la partida a partir del jugador ya actualizado, el daño recibido y el contacto con la meta
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDano  = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando
