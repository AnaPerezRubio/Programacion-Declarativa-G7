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

-- Definición del tipo Celda con data.
data Celda = Solido | Vacio | META | InicioEnemigo String
    deriving (Eq, Show)

data TipoEnemigo = Patrulla | Perseguidor
   deriving (Eq, Show)

data EstadoJuego = Jugando | PierdeVida | GameOver | NivelCompletado
   deriving (Eq, Show)

type Grid = [[Celda]]
type Vector2D = (Float, Float)
type BoundingBox = (Float, Float, Float, Float)

-- Convierte un caracter en su correspondiente tipo Celda.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte una lista de cadenas en una cuadrícula de celdas.
parsearGrid :: [String] -> Grid
parsearGrid = map (map parsearCelda)

-- Convierte una celda en su carácter correspondiente.
celdaAChar :: Celda -> Char
celdaAChar Solido            = '#'
celdaAChar Vacio             = '.'
celdaAChar META               = 'M'
celdaAChar (InicioEnemigo (c:_)) = c

-- Comprueba si una celda es sólida.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- Comprueba si una celda es la meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- Comprueba si una celda representa el inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False

-- Suma componente a componente dos vectores/puntos 2D.
sumaVectores:: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)
