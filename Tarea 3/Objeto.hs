-- =============================================================
-- OBJETO.hs
-- =============================================================

module Objeto (
    Objeto(..), Plataforma(..), Meta(..), Enemigo(..),
    solapan, colisionaCon, colisionaConAlguno
) where

import Tipos (Vector2D, BoundingBox,
              TipoEnemigo, sumaVectores,
              escalarVector)

class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id

data Plataforma = Plataforma BoundingBox 
  deriving (Eq, Show)
data Meta = Meta { posMeta :: Vector2D } 
  deriving (Eq, Show)

data Enemigo = Enemigo {ident :: Char, posEnemigo :: Vector2D,
                        velEnemigo :: Vector2D, dimE :: Vector2D,
                        tipo :: TipoEnemigo}
  deriving (Show, Eq)

instance Objeto Plataforma where
  posicion (Plataforma (x,y,w,h)) = (x,y)
  boundingBox (Plataforma caja) = caja
   
instance Objeto Meta where
  posicion = posMeta
  boundingBox m = (x,y,70,80)
    where (x,y) = posMeta m

instance Objeto Enemigo where
  posicion = posEnemigo
  boundingBox e = (x - w/2, y - h/2, w, h)
    where (x,y) = posEnemigo e
          (w,h) = dimE e
  animar dt e = e {posEnemigo = nuevaPos}
    where nuevaPos = sumaVectores p (escalarVector dt v)
          p = posEnemigo e
          v = velEnemigo e

-- Indica si dos cajas alineadas con los ejes se solapan.
solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, anc1, alt1) (x2, y2, anc2, alt2) = solapanX && solapanY
  where
    solapanX = x1 < x2 + anc2 && x1 + anc1 > x2
    solapanY = y1 < y2 + alt2 && y1 + alt1 > y2

-- Indica si un objeto ha colisionado con otro
colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon obj1 obj2 = solapan caja1 caja2
  where caja1 = boundingBox obj1
        caja2 = boundingBox obj2

-- Indica si un objeto ha colisionado con alguno de la lista dada
colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno obj = any (colisionaCon obj)