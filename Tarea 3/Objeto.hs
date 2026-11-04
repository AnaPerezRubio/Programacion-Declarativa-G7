module Objeto
    ( Objeto(..)
    , Plataforma(..)
    , Meta(..)
    , Enemigo(..)
    , solapan
    , colisionaCon
    , colisionaConAlguno
    ) where

import Tipos

class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id   

data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
  deriving (Eq, Show)

data Enemigo = Enemigo
    { idEnemigo       :: String
    , posEnemigo      :: Vector2D
    , velEnemigo      :: Vector2D
    , dimensionEnemigo :: Vector2D
    , tipoEnemigo     :: TipoEnemigo } 
    deriving (Eq, Show)

instance Objeto Plataforma where
    posicion (Plataforma (x,y,_,_)) = (x,y)
    boundingBox (Plataforma caja) = caja

instance Objeto Meta where
    posicion = posMeta
    boundingBox (Meta (x,y)) = (x,y,70,80)

instance Objeto Enemigo where
    posicion = posEnemigo
    boundingBox e =
        let (x,y) = posEnemigo e
            (w,h) = dimensionEnemigo e
        in (x-w/2, y-h/2, w, h)
    animar dt e =
        e {posEnemigo = sumaVectores (posEnemigo e) (escalarVector dt (velEnemigo e))}

-- Indica si dos cajas delimitadoras alineadas con los ejes se solapan.
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x2 < x1 + w1 &&
  y1 < y2 + h2 && y2 < y1 + h1

-- Comprueba si dos objetos tienen cajas de colisión solapadas.
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

-- Comprueba si un objeto colisiona con alguno de los objetos de una lista.
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a bs = any (colisionaCon a) bs