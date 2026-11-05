-- =============================================================
-- RENDER.hs
-- =============================================================

module Render (
    dibujarObjeto, dibujarHUD, mensajeEstado, dibujarMundo
) where

import Tipos (EstadoJuego(..))
import Objeto (Objeto(..))
import Jugador (Jugador, vidas, puntuacion)
import Mundo (Mundo(..))
import Graphics.Gloss

-- Dibuja un objeto a partir de su boundingBox
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col obj = 
  translate cx cy (color col (rectangleSolid w h))
  where
    (x, y, w, h) = boundingBox obj
    cx =  x + w / 2
    cy = y + h / 2

-- Dibuja el HUD de juego con las vidas y puntos actuales
dibujarHUD :: Jugador -> Picture
dibujarHUD j = translate (-380) 270 (scale 0.15 0.15 (text mensaje))
  where mensaje = "Vidas: " ++ show (vidas j) ++ "   Puntos: " ++ show (puntuacion j)

-- Muestra un mensaje según el estado de la partida
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando         = Nothing
mensajeEstado PierdeVida      = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver        = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO" 

-- Compone la escena, la cámara, el HUD y los mensajes de estado
dibujarMundo :: Mundo -> Picture
dibujarMundo mundo = Pictures (escenaDesplazada : hud : mensaje)
  where
    j = jugadorM mundo
    (px, py) = posicion j
    
    capaPlataformas = Pictures $ map (dibujarObjeto (greyN 0.5)) (plataformasM mundo)
    capaMetas       = Pictures $ map (dibujarObjeto green) (metasM mundo)
    capaEnemigos    = Pictures $ map (dibujarObjeto red) (enemigosM mundo)
    capaJugador     = dibujarObjeto blue j
    
    escena = Pictures [capaPlataformas, capaMetas, capaEnemigos, capaJugador]
    escenaDesplazada = translate (-px) (-py) escena
    
    hud = dibujarHUD j
    
    mensaje = case mensajeEstado (estadoM mundo) of
                Nothing  -> []
                Just msj -> [translate (-250) 0 (scale 0.3 0.3 (color black (text msj)))]
