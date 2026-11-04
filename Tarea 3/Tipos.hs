-- =============================================================
-- TIPOS.hs
-- =============================================================

module Tipos (
    Vector2D, BoundingBox, 
    sumaVectores, escalarVector, 
    Celda(..), Grid, TipoEnemigo(..), EstadoJuego(..),
    parsearCelda, parsearGrid, celdaAChar, esSolido, esMeta, esEnemigo
) where

type Vector2D = (Float, Float)
type BoundingBox = (Float, Float, Float, Float)

data Celda = Vacio | Solido | InicioEnemigo Char | META 
  deriving (Show, Read, Eq)
type Grid = [[Celda]]

data TipoEnemigo = Patrulla | Perseguidor 
  deriving (Show, Eq)
data EstadoJuego = Jugando | PierdeVida | Gameover | NivelCompletado 
  deriving (Show, Eq)

-- Suma componente a componente dos vectores/puntos 2D.
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)

-- Convierte un Char en una Celda del tipo que corresponda
parsearCelda :: Char -> Celda
parsearCelda '.' = Vacio
parsearCelda '#' = Solido
parsearCelda 'M' = META
parsearCelda x   = InicioEnemigo x

-- Convierte una lista de String en un Grid
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- Convierte una Celda en un Char
celdaAChar :: Celda -> Char
celdaAChar Vacio             = '.'
celdaAChar Solido            = '#'
celdaAChar META              = 'M'
celdaAChar (InicioEnemigo c) = c

-- Decide si una celda es solida.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- Decide si una celda es meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- Decide si una celda es un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False