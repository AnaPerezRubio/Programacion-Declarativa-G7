# Organización del proyecto en módulos

Hemos dividido el proyecto en varios módulos para tener el código mejor organizado. Cada módulo se encarga de una parte del juego y contiene las funciones y tipos relacionados con ella.

Una lista de exportación sirve para indicar qué funciones y tipos se pueden utilizar desde otros módulos. Así no hace falta que todos los módulos puedan acceder a todo el código.

En el módulo Jugador hemos ocultado el constructor y solo exportamos el tipo Jugador junto con los selectores y las funciones que necesitamos. De esta forma, desde otros módulos no se puede crear un jugador directamente. Para hacerlo hay que utilizar crearJugador.

Esto también ayuda a evitar errores como crear un jugador con vidas negativas o modificar sus datos directamente. Así controlamos mejor cómo se crea y cómo se modifica el jugador.