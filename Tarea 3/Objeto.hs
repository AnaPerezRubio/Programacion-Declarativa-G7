-- clase Objeto y objetos del juego
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

-- cosas que tienen posicion y caja de colision
class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id

-- data de plataforma
data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

-- data de meta del nivel
data Meta = Meta { posMeta :: Vector2D }
  deriving (Eq, Show)

-- data del enemigo
data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

-- funcion que dice si dos cajas se solapan
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, anc1, alt1) (x2, y2, anc2, alt2) = solapanX && solapanY
  where
    solapanX = x1 < x2 + anc2 && x1 + anc1 > x2
    solapanY = y1 < y2 + alt2 && y1 + alt1 > y2

-- instancia para Plataforma
instance Objeto Plataforma where
  posicion (Plataforma (x, y, _, _)) = (x, y)
  boundingBox (Plataforma b)         = b

-- instancia para Meta
instance Objeto Meta where
  posicion    = posMeta
  boundingBox (Meta (x, y)) = (x, y, 70, 80)

-- instancia para Enemigo (animar lo mueve)
instance Objeto Enemigo where
  posicion = posEnemigo
  boundingBox e = (x - w / 2, y - h / 2, w, h)
    where
      (x, y) = posEnemigo e
      (w, h) = dimEnemigo e
  animar dt e = e { posEnemigo = sumaVectores (posEnemigo e) (escalarVector dt (velEnemigo e)) }

-- funcion que dice si dos objetos chocan
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

-- funcion que dice si un objeto choca con alguno de la lista
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a = any (colisionaCon a)
