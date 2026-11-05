-- Programa principal (dado por el enunciado): el único código con IO
module Main (main) where

import Graphics.Gloss
import Nivel
import Mundo
import Render

-- Nivel de prueba incluido en el código como lista de líneas
nivel1Lineas :: [String]
nivel1Lineas =
  [ "NOMBRE: Colinas"
  , "FONDOS: 1,2,3"
  , "---"
  , "..................M"
  , "..........A........"
  , "####.....####.#####"
  ]

-- Ventana de Gloss
ventana :: Display
ventana = InWindow "Plataformas" (800, 600) (100, 100)

-- Lanza el juego, o muestra en la ventana el error si el nivel está mal formado
main :: IO ()
main = case parsearNivelCompleto nivel1Lineas of
  Left err -> display ventana white (scale 0.2 0.2 (text (show err)))
  Right nd -> play ventana white 60 (mundoInicial nd) dibujarMundo manejarEvento actualizarMundo
