-- tipos basicos del proyecto.
module Tipos
  ( Vector2D
  , BoundingBox
  , sumaVectores
  , escalarVector
  , Celda(..)
  , Grid
  , TipoEnemigo(..)
  , EstadoJuego(..)
  , parsearCelda
  , parsearGrid
  , celdaAChar
  , esSolido
  , esMeta
  , esEnemigo
  ) where

-- vector o punto 2D
type Vector2D = (Float, Float)

-- caja de colision (x, y, ancho, alto)
type BoundingBox = (Float, Float, Float, Float)

-- tipos de celda del mapa
data Celda
  = Solido
  | Vacio
  | META
  | InicioEnemigo String
  deriving (Eq, Show)

-- mapa: lista de filas de celdas
type Grid = [[Celda]]

-- tipos de enemigo
data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

-- estados del juego
data EstadoJuego = Jugando | PierdeVida | GameOver | NivelCompletado
  deriving (Eq, Show)

-- funcion que suma dos vectores
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- funcion que multiplica un vector por un numero
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)

-- funcion que pasa un caracter a celda
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- funcion que pasa las lineas de texto a un mapa
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- funcion que pasa una celda a caracter
celdaAChar :: Celda -> Char
celdaAChar Solido                = '#'
celdaAChar Vacio                 = '.'
celdaAChar META                  = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo [])    = '?'

-- funcion que dice si la celda es solida
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- funcion que dice si la celda es la meta
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- funcion que dice si la celda es un enemigo
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False
