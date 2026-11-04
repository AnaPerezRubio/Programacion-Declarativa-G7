module Jugador ( Jugador, posJugador, velJugador, vidas, puntuacion, crearJugador, perderVida, sumarPuntos, conVelocidad, moverJugador, estadoSiguiente) where

import Tipos
import Objeto

data Jugador = Jugador
    { posJugador  :: Vector2D
    , velJugador  :: Vector2D
    , vidas       :: Int
    , puntuacion  :: Int } 
    deriving (Eq, Show)

instance Objeto Jugador where
    posicion = posJugador
    boundingBox j = 
        let (x,y) = posJugador j
        in (x - 30, y, 60, 90)

-- Crea un jugador con sus valores iniciales.
crearJugador :: Vector2D -> Jugador
crearJugador pos = Jugador pos (0,0) 3 0

-- Resta una vida al jugador sin bajar de cero.
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Suma puntos a la puntuación actual del jugador.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos p j = j { puntuacion = puntuacion j + p }

-- Establece una nueva velocidad para el jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad v j = j { velJugador = v }

-- Mueve al jugador según su velocidad y el tiempo transcurrido.
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j = j { posJugador = sumaVectores (posJugador j) (escalarVector dt (velJugador j)) }

-- Determina el estado de la partida según las vidas, el daño y la meta.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDaño tocaMeta
    | vidas j <= 0 = GameOver
    | recibioDaño = PierdeVida
    | tocaMeta = NivelCompletado
    | otherwise = Jugando