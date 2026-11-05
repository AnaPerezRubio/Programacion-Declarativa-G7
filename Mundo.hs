-- Estado del mundo del juego y su paso de simulación (tiempo y teclado)
module Mundo
  ( Mundo(..)
  , mundoInicial
  , actualizarMundo
  , manejarEvento
  ) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), KeyState(..), SpecialKey(..))
import Jugador (Jugador, conVelocidad, crearJugador, estadoSiguiente, moverJugador, perderVida, velJugador)
import Nivel (NivelData, enemigosDesdeGrid, gridAPlataformas, gridNivel, metasDesdeGrid)
import Objeto (Enemigo, Meta, Plataforma, animar, colisionaConAlguno)
import Tipos (EstadoJuego(..), Vector2D)

-- Todo lo que existe en el mundo durante una partida
data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

-- Crea el mundo inicial de un nivel
mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM     = crearJugador (60, 120)
  , enemigosM    = enemigosDesdeGrid g
  , plataformasM = gridAPlataformas g
  , metasM       = metasDesdeGrid g
  , estadoM      = Jugando
  , nivelM       = nd
  }
  where g = gridNivel nd

-- Avanza dt segundos: mueve al jugador y a los enemigos, detecta daño y meta y actualiza el estado
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
  | estadoM m /= Jugando = m
  | otherwise = m { jugadorM = j', enemigosM = enemigos', estadoM = estadoSiguiente j' dano tocaMeta }
  where
    j         = moverJugador dt (jugadorM m)
    enemigos' = map (animar dt) (enemigosM m)
    dano      = colisionaConAlguno j enemigos'
    tocaMeta  = colisionaConAlguno j (metasM m)
    j'        = if dano then perderVida j else j

-- Modifica la velocidad del jugador aplicando una función a la velocidad actual
cambiaVel :: (Vector2D -> Vector2D) -> Mundo -> Mundo
cambiaVel f m = m { jugadorM = conVelocidad (f (velJugador j)) j }
  where j = jugadorM m

-- Reacciona al teclado: flechas fijan o anulan la velocidad, 'r' reinicia el nivel
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = cambiaVel (\(_, vy) -> (100, vy)) m
manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) m = cambiaVel (\(_, vy) -> (-100, vy)) m
manejarEvento (EventKey (SpecialKey KeyUp)    Down _ _) m = cambiaVel (\(vx, _) -> (vx, 100)) m
manejarEvento (EventKey (SpecialKey KeyDown)  Down _ _) m = cambiaVel (\(vx, _) -> (vx, -100)) m
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _)   m = cambiaVel (\(_, vy) -> (0, vy)) m
manejarEvento (EventKey (SpecialKey KeyLeft)  Up _ _)   m = cambiaVel (\(_, vy) -> (0, vy)) m
manejarEvento (EventKey (SpecialKey KeyUp)    Up _ _)   m = cambiaVel (\(vx, _) -> (vx, 0)) m
manejarEvento (EventKey (SpecialKey KeyDown)  Up _ _)   m = cambiaVel (\(vx, _) -> (vx, 0)) m
manejarEvento (EventKey (Char 'r') Down _ _)            m = mundoInicial (nivelM m)
manejarEvento _                                         m = m
