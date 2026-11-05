# Organización del proyecto en módulos

**Qué es un módulo.** Un módulo es un fichero `.hs` (con el mismo nombre que el módulo) que agrupa
tipos y funciones relacionados. Permite dividir un programa grande en piezas con una responsabilidad
clara (`Tipos`, `Objeto`, `Jugador`, `Nivel`, `Partida`, `Mundo`, `Render`, `Main`), reutilizarlas y
compilarlas por separado. Los módulos se conectan con `import`; aquí las dependencias no tienen ciclos
y `Tipos` no depende de ningún otro módulo propio.

**Qué es una lista de exportación.** Es la lista entre paréntesis tras `module Nombre`, que dice qué
nombres (funciones, tipos, constructores) ven los demás módulos. Todo lo que no aparece queda oculto
y es un detalle interno. Por ejemplo, `Nivel` oculta `aEither`, `separarMapa` y `celdasConPosicion`,
que solo son ayudas internas, y `Partida` oculta `leerCampo`. Así se puede cambiar la implementación
interna sin romper al resto del código.

**Por qué ocultar el constructor de `Jugador`.** `Jugador` se exporta sin constructor (`Jugador`, no
`Jugador(..)`), es decir, como tipo abstracto. Fuera del módulo nadie puede escribir
`Jugador (0,0) (0,0) (-5) 0` ni usar actualización de registro `j { vidas = -5 }`; la única forma de
obtener un jugador es `crearJugador` (3 vidas) y la única de modificarlo son `perderVida`, `sumarPuntos`,
`conVelocidad` y `moverJugador`. Como `perderVida` usa `max 0`, se garantiza la invariante «las vidas
nunca son negativas»: el compilador impide construir un jugador que la incumpla. Los selectores
(`vidas`, `posJugador`...) sí se exportan para poder consultarlo.

**Compilar y ejecutar:** `ghc --make Main.hs` (o `runhaskell Main.hs`), con la librería `gloss`.
