-- Dibujo del mundo con Gloss (todo son funciones puras que devuelven Picture)
module Render
  ( dibujarObjeto
  , dibujarHUD
  , mensajeEstado
  , dibujarMundo
  ) where

import Graphics.Gloss (Color, Picture, black, blue, color, green, greyN, pictures, rectangleSolid, red, scale, text, translate)
import Jugador (Jugador, posJugador, puntuacion, vidas)
import Mundo (Mundo(..))
import Objeto (Objeto, boundingBox)
import Tipos (EstadoJuego(..))

-- Dibuja un rectángulo sólido de color col con el tamaño y posición de la caja del objeto
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col o = translate (x + w / 2) (y + h / 2) (color col (rectangleSolid w h))
  where (x, y, w, h) = boundingBox o

-- Dibuja el texto fijo con las vidas y los puntos del jugador
dibujarHUD :: Jugador -> Picture
dibujarHUD j = translate (-380) 270 (scale 0.15 0.15 (text txt))
  where txt = "Vidas: " ++ show (vidas j) ++ "   Puntos: " ++ show (puntuacion j)

-- Mensaje a mostrar según el estado (Nothing mientras se juega)
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando         = Nothing
mensajeEstado PierdeVida      = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver        = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

-- Compone escena (con cámara siguiendo al jugador), HUD fijo y mensaje de estado centrado
dibujarMundo :: Mundo -> Picture
dibujarMundo m = pictures (escena : dibujarHUD jugador : mensaje)
  where
    jugador   = jugadorM m
    (px, py)  = posJugador jugador
    escena    = translate (-px) (-py) (pictures
                  [ pictures (map (dibujarObjeto (greyN 0.5)) (plataformasM m))
                  , pictures (map (dibujarObjeto green) (metasM m))
                  , pictures (map (dibujarObjeto red) (enemigosM m))
                  , dibujarObjeto blue jugador
                  ])
    mensaje   = maybe [] (\txt -> [translate (-250) 0 (scale 0.3 0.3 (color black (text txt)))])
                         (mensajeEstado (estadoM m))
