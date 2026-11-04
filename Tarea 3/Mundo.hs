-- =============================================================
-- MUNDO.hs
-- =============================================================

module Mundo (
    Mundo(..), mundoInicial, actualizarMundo, manejarEvento
) where

import Tipos (Vector2D, EstadoJuego(..))
import Objeto (Objeto(..), Plataforma(..), Meta(..), Enemigo(..), colisionaConAlguno)
import Jugador (Jugador, velJugador, crearJugador,
                moverJugador, perderVida, estadoSiguiente)
import Nivel (NivelData(..), gridAPlataformas, metasDesdeGrid, enemigosDesdeGrid)
import Graphics.Gloss.Interface.Pure.Game

data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

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

-- En caso de estar jugando: mueve al jugador, anima los enemigos
-- calcula colisiones, efectúa daño y comprueba la llegada a la meta.
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
  | estadoM m /= Jugando = m
  | otherwise = m { jugadorM = jFinal, enemigosM = esAnimados, estadoM = stFinal }
  where
    j = jugadorM m
    jMovido = moverJugador dt j
    esAnimados = map (animar dt) (enemigosM m)
    
    danyo = colisionaConAlguno jMovido esAnimados
    meta = colisionaConAlguno jMovido (metasM m)
    
    jFinal = if danyo then perderVida jMovido else jMovido
    stFinal = estadoSiguiente jFinal danyo meta


manejarEvento :: Event -> Mundo -> Mundo
-- Flechas horizontales
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = actualizarVelocidad (\(_, vy) -> (-100, vy)) m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = actualizarVelocidad (\(_, vy) -> (100, vy)) m
manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m = actualizarVelocidad (\(_, vy) -> (0, vy)) m
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) m = actualizarVelocidad (\(_, vy) -> (0, vy)) m

-- Flechas verticales
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m = actualizarVelocidad (\(vx, _) -> (vx, 100)) m
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m = actualizarVelocidad (\(vx, _) -> (vx, -100)) m
manejarEvento (EventKey (SpecialKey KeyUp) Up _ _) m = actualizarVelocidad (\(vx, _) -> (vx, 0)) m
manejarEvento (EventKey (SpecialKey KeyDown) Up _ _) m = actualizarVelocidad (\(vx, _) -> (vx, 0)) m

-- Reinicio de nivel
manejarEvento (EventKey (Char 'r') Down _ _) m = mundoInicial (nivelM m)
manejarEvento (EventKey (Char 'R') Down _ _) m = mundoInicial (nivelM m)

-- Cualquier otro evento no cambia el mundo
manejarEvento _ m = m

-- Función auxiliar que modifica solo las componentes deseadas
actualizarVelocidad :: (Vector2D -> Vector2D) -> Mundo -> Mundo
actualizarVelocidad f m = m { jugadorM = j { velJugador = nuevaVel } }
  where 
    j = jugadorM m
    nuevaVel = f (velJugador j)
