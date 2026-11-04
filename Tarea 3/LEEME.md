# Programación Declarativa - Tarea 3

## Definición de módulo.
Un módulo en Haskell es un grupo de funciones, tipos de datos y clases que se almacenan juntas en un archivo independiente, ya que comparten algún tipo de relación.
La creación de módulos permite dividir un programa extenso en subpartes más pequeñas, lo cual hace el proyecto más entendible, más mantenible y permite la reutilización de codigo.

```haskell
module Main (main) where
```

## Definición de lista de exportación.
Se trata de una declaración al comienzo del módulo que expresa los elementos públicos del archivo, es decir, cuáles de ellos son importables por otros módulos.
Lo que no se inluye en esta lista queda oculto para los programas que lo importan, garantizando así la no modificación de los elementos que consideremos críticos.

```haskell
module Tipos (
    Vector2D, BoundingBox, 
    sumaVectores, escalarVector, 
    Celda(..), Grid, TipoEnemigo(..), EstadoJuego(..),
    parsearCelda, parsearGrid, celdaAChar, esSolido, esMeta, esEnemigo
) where
```

## El por qué de ocultar el constructor de Jugador.
En el módulo `Jugador` se ha considerando la siguiente lista de exportación.

```haskell
module Jugador (
    Jugador, posJugador, velJugador, vidas, puntuacion, 
    crearJugador, perderVida, sumarPuntos, conVelocidad,
    moverJugador, estadoSiguiente
) where

```

Como puede verse, se ha omitido la importación del constructor (`Jugadro(..)`) pero sí se han declarado sus funciones selectoras.
Esto tiene el propósito de que los demás módulos puedan leer los datos de un jugador y trabajar con el tipo, pero no puedan instanciar un nuevo jugador, forzando el uso de la función `crearJugador` para esto.
De no convertirlo en un tipo de dato abstracto, otro módulo podría construir un `Jugador` con vidas negativas o infinitas, algo que rompe totalmente la lógica del juego. De esta forma, estamos garantizando la seguridad y consistencia de los datos del trabajo.

---
Ana Pérez Rubio - Curso 2026/27