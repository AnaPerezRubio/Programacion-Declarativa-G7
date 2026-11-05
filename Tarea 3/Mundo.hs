-- modulo del mundo del juego
module Mundo
  ( Mundo(..)
  , mundoInicial
  , actualizarMundo
  , manejarEvento
  ) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), KeyState(..), SpecialKey(..))

import Jugador
import Nivel (NivelData(..), enemigosDesdeGrid, gridAPlataformas, metasDesdeGrid)
import Objeto (Enemigo, Meta, Plataforma, animar, colisionaConAlguno)
import Tipos (EstadoJuego(..))

-- datos del mundo
data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

-- funcion que crea el mundo de un nivel
mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM = crearJugador (60, 120)
  , enemigosM = enemigosDesdeGrid g
  , plataformasM = gridAPlataformas g
  , metasM = metasDesdeGrid g
  , estadoM = Jugando
  , nivelM = nd
  }
  where g = gridNivel nd

-- Avanza el mundo dt segundos.
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
  | estadoM m /= Jugando = m
  | otherwise = m { jugadorM = jugadorFinal, enemigosM = enemigos', estadoM = nuevoEstado }
  where
    jugadorMovido = moverJugador dt (jugadorM m)
    enemigos'     = map (animar dt) (enemigosM m)
    hayDanio      = colisionaConAlguno jugadorMovido enemigos'
    tocaMeta      = colisionaConAlguno jugadorMovido (metasM m)
    jugadorFinal  = if hayDanio then perderVida jugadorMovido else jugadorMovido
    nuevoEstado   = estadoSiguiente jugadorFinal hayDanio tocaMeta

-- funcion que cambia la velocidad horizontal
conVelX :: Float -> Mundo -> Mundo
conVelX vx m = m { jugadorM = conVelocidad (vx, snd (velJugador j)) j }
  where j = jugadorM m

-- funcion que cambia la velocidad vertical
conVelY :: Float -> Mundo -> Mundo
conVelY vy m = m { jugadorM = conVelocidad (fst (velJugador j), vy) j }
  where j = jugadorM m

-- funcion que sirve para crear los eventos con el teclado
manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) = conVelX (-100)
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) = conVelX 100
manejarEvento (EventKey (SpecialKey KeyUp)    Down _ _) = conVelY 100
manejarEvento (EventKey (SpecialKey KeyDown)  Down _ _) = conVelY (-100)
manejarEvento (EventKey (SpecialKey KeyLeft)  Up _ _)   = conVelX 0
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _)   = conVelX 0
manejarEvento (EventKey (SpecialKey KeyUp)    Up _ _)   = conVelY 0
manejarEvento (EventKey (SpecialKey KeyDown)  Up _ _)   = conVelY 0
manejarEvento (EventKey (Char 'r') Down _ _)            = \m -> mundoInicial (nivelM m)
manejarEvento _                                         = id
