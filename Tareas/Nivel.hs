module Nivel where

import Test.QuickCheck 

-----------------------------------
-- Declaraciones de tipos sinónimos 
------------------------------------

type Vector2D = (Double, Double)
type Caja = (Double, Double, Double, Double) -- (x, y, ancho, alto)
type Lado = String
type Celda = Char
type Fila = String
type Nivel = [Fila]
type Posicion = (Int, Int)
type Enemigo = (Int, Int, Char)


-------------------
-- 1. Vectores 2D
-------------------

-- Suma componente a componente dos vectores/puntos 2D.
sumaVectores:: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Double -> Vector2D -> Vector2D
escalarVector k (x, y) = (k * x, k * y)

-- Calcula la distancia euclídea entre dos puntos 2D.
distancia :: Vector2D -> Vector2D -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-------------------------
-- 2. Cajas de colisión
-------------------------

-- Indica si dos cajas delimitadoras alineadas con los ejes se solapan.
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x2 < x1 + w1 &&
  y1 < y2 + h2 && y2 < y1 + h1

-------------------------
-- 3. Lado de colisión
-------------------------

-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto.
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
  | sArriba <= sAbajo && sArriba <= sIzq && sArriba <= sDer  = "Arriba"
  | sAbajo <= sIzq && sAbajo <= sDer                         = "Abajo"
  | sIzq <= sDer                                             = "Izquierda"
  | otherwise                                                = "Derecha"
  where
    sArriba = (y1 + h1) - y2
    sAbajo  = (y2 + h2) - y1
    sIzq    = (x1 + w1) - x2
    sDer    = (x2 + w2) - x1

--------------------------------------
-- 4. Utilidades de listas y cadenas
--------------------------------------

-- Divide una cadena en trozos cada vez que aparece un carácter separador dado.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
  | x == sep  = "" : resto
  | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

-- Elimina los espacios en blanco al principio y al final de una cadena.
trim :: String -> String
trim = dropWhile esEspacio . reverse . dropWhile esEspacio . reverse
  where
    esEspacio c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- Cuenta cuántos elementos de una lista cumplen una condición dada.
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]

-- Convierte una lista de dos (o más) números en un vector/punto 2D.
list2Vector2D :: [Double] -> Vector2D
list2Vector2D (x:y:_) = (x, y)
list2Vector2D [_]     = error "Falta un elemento en la lista para convertir en Vector2D"
list2Vector2D []      = error "Lista vacia, no es posible convertir en Vector2D"

-----------------------
-- 5. Parseo del nivel
-----------------------

-- Convierte la lista de líneas de texto en la estructura de datos que representa el nivel.
parsearNivel :: [String] -> Nivel
parsearNivel = id

-- Indica si una celda es una plataforma sólida.
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

-- Indica si una celda es la meta del nivel.
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

-- Indica si una celda está vacía.
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

-- Indica si una celda marca el punto de inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c) && not (esMeta c) && not (esVacio c)

-- Devuelve la lista de posiciones (fila, columna) en las que aparece la meta.
posicionesMeta :: Nivel -> [Posicion]
posicionesMeta grid =
  [ (r, c) | (r, fila) <- zip [0..] grid
           , (c, celda) <- zip [0..] fila
           , esMeta celda ]

-- Devuelve la lista de posiciones e identificadores de cada enemigo (fila, columna, id).
posicionesEnemigos :: Nivel -> [Enemigo]
posicionesEnemigos grid =
  [ (r, c, celda) | (r, fila) <- zip [0..] grid
                  , (c, celda) <- zip [0..] fila
                  , esEnemigo celda ]

-- Recorre una fila del nivel y agrupa las columnas '#' consecutivas en pares (columna_inicio, longitud).
agruparRachas :: Fila -> [(Int, Int)]
agruparRachas [] = []
agruparRachas fila
  | null rachasSolidas = []
  | otherwise          = (colInicio, length rachasSolidas) : [ (c + colFinRacha, len) | (c, len) <- agruparRachas resto ]
  where
    -- 1. Separamos los espacios iniciales del resto
    (espacios, trasEspacios) = span (not . esSolido) fila
    -- 2. Separamos el bloque sólido de lo que viene después
    (rachasSolidas, resto)   = span esSolido trasEspacios
    colInicio   = length espacios
    colFinRacha = colInicio + length rachasSolidas
    
-------------------------------------
-- Bonus: Propiedades con QuickCheck
-------------------------------------

-- Propiedad 1: Conmutatividad de la suma
prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- Propiedad 2: Asociatividad de la suma
prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)

-- Propiedad 3: Elemento neutro del producto escalar
prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- Propiedad 4: No negatividad de la distancia
prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- Propiedad 5: Simetría de la distancia
prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- Propiedad 6: Simetría del solape de cajas
prop_solapan_simetrica :: Caja -> Caja -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- Propiedad 7: Idempotencia de la función trim
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- Propiedad 8: splitOn devuelve un único elemento si el separador no existe en la cadena
prop_split0n_sin_separador :: Char -> String -> Property
prop_split0n_sin_separador sep s = notElem sep s ==> splitOn sep s == [s]

-- Propiedad 9: contarSiCumple nunca devuelve una cantidad superior a la longitud de la lista
prop_contarSiCumple_acotado :: String -> Bool
prop_contarSiCumple_acotado xs = contarSiCumple (== 'a') xs <= length xs