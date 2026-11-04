-- Clase de los objetos con presencia en el mundo y los objetos que no son el jugador
module Objeto
  ( Objeto(..)
  , Plataforma(..)
  , Meta(..)
  , Enemigo(..)
  , solapan
  , colisionaCon
  , colisionaConAlguno
  ) where

import Tipos (BoundingBox, TipoEnemigo, Vector2D, escalarVector, sumaVectores)

-- Objetos con posición y caja de colisión; animar no hace nada por defecto
class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id

-- Plataforma sólida definida por su caja
data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

-- Meta del nivel, situada por su esquina inferior izquierda
data Meta = Meta { posMeta :: Vector2D }
  deriving (Eq, Show)

-- Enemigo con identificador, posición (centro), velocidad, dimensiones y tipo
data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

instance Objeto Plataforma where
  posicion (Plataforma (x, y, _, _)) = (x, y)
  boundingBox (Plataforma bb)        = bb

instance Objeto Meta where
  posicion                = posMeta
  boundingBox (Meta (x, y)) = (x, y, 70, 80)

instance Objeto Enemigo where
  posicion = posEnemigo
  boundingBox e = (x - w / 2, y - h / 2, w, h)
    where (x, y) = posEnemigo e
          (w, h) = dimEnemigo e
  animar dt e = e { posEnemigo = sumaVectores (posEnemigo e) (escalarVector dt (velEnemigo e)) }

-- Comprueba si dos cajas (x, y, ancho, alto) se solapan
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

-- Comprueba si las cajas de dos objetos cualesquiera se solapan
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon o1 o2 = solapan (boundingBox o1) (boundingBox o2)

-- Comprueba si un objeto colisiona con algún objeto de una lista
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno o = any (colisionaCon o)
