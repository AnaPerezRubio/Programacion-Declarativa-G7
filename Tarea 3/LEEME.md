# Tarea 3 — LEEME

Un modulo es un fichero .hs que agrupa definiciones (tipos, clases, funciones).
Sirve para repartir el programa en piezas con una responsabilidad clara (Tipos, Objeto, Nivel, Mundo, Render...) y para que otros módulos las reutilicen con import. Las dependencias no pueden formar ciclos.

Una lista de exportacion es la lista que va tras module Nombre (...) where y dice qué nombres son visibles desde fuera. Todo lo que no aparece queda privado, asi que se pueden usar funciones auxiliares internas (como buscarOError en Nivel) sin que formen parte de la interfaz pública del módulo.

Por qué ocultar el constructor de Jugador? Jugador se exporta sin constructor (Jugador, no Jugador(..)).
Fuera de su módulo nadie puede escribir Jugador (0,0) (0,0) (-5) 0, así que la unica forma de obtener un jugador es crearJugador (3 vidas, 0 puntos), y la única forma de cambiarlo son funciones como perderVida, que nunca bajan de 0 vidas.
De ese modo la invariante «las vidas nunca son negativas» la garantiza un solo módulo.