-- dibujo del juego
module Render 
  ( dibujarObjeto
  , dibujarHUD
  , mensajeEstado
  , dibujarMundo
  ) where

import Graphics.Gloss (Color, Picture, black, blue, color, green, greyN, pictures, rectangleSolid, red, scale, text, translate)

import Jugador (Jugador, puntuacion, vidas)
import Mundo (Mundo(..))
import Objeto (Objeto(..))
import Tipos (EstadoJuego(..))

-- funcion que dibuja un objeto como rectangulo
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col o = translate (x + w / 2) (y + h / 2) (color col (rectangleSolid w h))
  where (x, y, w, h) = boundingBox o

-- funcion que dibuja vidas y puntos
dibujarHUD :: Jugador -> Picture
dibujarHUD j =
  translate (-380) 270 (scale 0.15 0.15 (text ("Vidas: " ++ show (vidas j) ++ "   Puntos: " ++ show (puntuacion j))))

-- funcion que devuelve un mensaje segun el estado
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando         = Nothing
mensajeEstado PierdeVida      = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver        = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

-- funcion que dibuja todo el mundo
dibujarMundo :: Mundo -> Picture
dibujarMundo m = pictures ([escena, dibujarHUD j] ++ mensaje)
  where
    j = jugadorM m
    (px, py) = posicion j
    escena = translate (-px) (-py) (pictures
      [ pictures (map (dibujarObjeto (greyN 0.5)) (plataformasM m))
      , pictures (map (dibujarObjeto green) (metasM m))
      , pictures (map (dibujarObjeto red) (enemigosM m))
      , dibujarObjeto blue j
      ])
    mensaje = case mensajeEstado (estadoM m) of
      Nothing  -> []
      Just msg -> [translate (-250) 0 (scale 0.3 0.3 (color black (text msg)))]
