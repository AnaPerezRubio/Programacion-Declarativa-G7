-- Tipos básicos compartidos por todo el proyecto (no depende de ningún otro módulo propio)
module Tipos
  ( Vector2D
  , BoundingBox
  , Grid
  , Celda(..)
  , TipoEnemigo(..)
  , EstadoJuego(..)
  , sumaVectores
  , escalarVector
  , parsearCelda
  , parsearGrid
  , celdaAChar
  , esSolido
  , esMeta
  , esEnemigo
  ) where

-- Vector o punto del plano (x, y)
type Vector2D = (Float, Float)

-- Caja delimitadora (x, y, ancho, alto), con (x, y) en la esquina inferior izquierda
type BoundingBox = (Float, Float, Float, Float)

-- Mapa de un nivel: lista de filas de celdas (la primera fila es la más alta)
type Grid = [[Celda]]

-- Casuísticas posibles de una celda del mapa
data Celda = Vacio | Solido | META | InicioEnemigo String
  deriving (Eq, Show)

-- Tipos de enemigo según su dinámica de movimiento
data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

-- Estado en el que se encuentra la partida
data EstadoJuego = Jugando | PierdeVida | GameOver | NivelCompletado
  deriving (Eq, Show)

-- Suma dos vectores componente a componente
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector por un escalar
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector s (x, y) = (s * x, s * y)

-- Convierte un carácter en la celda correspondiente ('.' vacío, '#' sólido, 'M' meta, otro: enemigo)
parsearCelda :: Char -> Celda
parsearCelda '.' = Vacio
parsearCelda '#' = Solido
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte una lista de cadenas en un Grid
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- Operación inversa de parsearCelda (un enemigo sin identificador se escribe como vacío)
celdaAChar :: Celda -> Char
celdaAChar Vacio                 = '.'
celdaAChar Solido                = '#'
celdaAChar META                  = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo [])    = '.'

-- Indica si la celda es sólida
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- Indica si la celda es la meta
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- Indica si la celda es el punto de aparición de un enemigo
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False
