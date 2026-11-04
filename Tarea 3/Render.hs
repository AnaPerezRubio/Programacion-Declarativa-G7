module Render
    ( dibujarObjeto
    , dibujarHUD
    , mensajeEstado
    , dibujarMundo
    ) where

import Tipos
import Objeto
import Jugador
import Mundo
import Graphics.Gloss

-- Dibuja un objeto como un rectángulo sólido del color indicado.
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col o =
    translate (x + w/2) (y + h/2) (color col (rectangleSolid w h))
    where
        (x, y, w, h) = boundingBox o

-- Dibuja el HUD con las vidas y los puntos del jugador.
dibujarHUD :: Jugador -> Picture
dibujarHUD j =
    translate (-380) 270
        (scale 0.15 0.15
            (text ("Vidas: " ++ show (vidas j) ++
                   "   Puntos: " ++ show (puntuacion j))))

-- Devuelve el mensaje correspondiente al estado actual de la partida.
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando = Nothing
mensajeEstado PierdeVida = Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver = Just "GAME OVER"
mensajeEstado NivelCompletado = Just "NIVEL COMPLETADO"

-- Dibuja el mundo completo con la cámara siguiendo al jugador, el HUD y el mensaje de estado.
dibujarMundo :: Mundo -> Picture
dibujarMundo m =
    Pictures
        [ translate (-px) (-py) escena
        , dibujarHUD (jugadorM m)
        , case mensajeEstado (estadoM m) of
            Nothing -> Blank
            Just msg -> translate (-250) 0 (scale 0.3 0.3 (color black (text msg)))
        ]
    where
        (px, py) = posJugador (jugadorM m)
        escena = Pictures
            ( map (dibujarObjeto (greyN 0.5)) (plataformasM m)
            ++ map (dibujarObjeto green) (metasM m)
            ++ map (dibujarObjeto red) (enemigosM m)
            ++ [dibujarObjeto blue (jugadorM m)]
            )