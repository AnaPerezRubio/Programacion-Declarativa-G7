--------------------------------------------
--	TAREA 1
-- 	GRUPO 7 	PROGRAMACIÓN DECLARATIVA
--------------------------------------------

import Test.QuickCheck 

-- TIPOS SINÓNIMOS

type Vector2 = (Double, Double)
type Caja = (Double, Double, Double, Double)
type Celda = Char
type Fila = [Celda]
type Grid = [Fila]
type Posicion = (Int, Int)
type Enemigo = (Int, Int, Char)

--------------------------------------------

-- VECTORES EN 2D

-- Función: sumaVectores
-- Suma componente a componente dos vectores/puntos 2D
sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- Función: escalarVector
-- Multiplica un vector/punto 2D por un factor escalar
escalarVector :: Double -> Vector2 -> Vector2
escalarVector k (x, y) = (k * x, k * y)


-- Función: distancia
-- Calcula la distancia euclídea entre dos vectores 2D
distancia :: Vector2 -> Vector2 -> Double
distancia (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1

--------------------------------------------

-- CAJAS DE COLISIÓN

-- Función: solapan
-- Indica si dos cajas delimitadoras solapan (colisionan)
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, anc1, alt1) (x2, y2, anc2, alt2) = solapanX && solapanY
  where
    solapanX = x1 < x2 + anc2 && x1 + anc1 > x2
    solapanY = y1 < y2 + alt2 && y1 + alt1 > y2

--------------------------------------------

-- SOLAPAMIENTO DE CAJAS

-- Función: ladoColision
-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto.
ladoColision :: Caja -> Caja -> String
ladoColision (x1,y1,b1,h1) (x2,y2,b2,h2)
 | choqueIzq == solapaMinimo = "Izquierda"
 | choqueDch == solapaMinimo = "Derecha"
 | choqueArr == solapaMinimo = "Arriba"
 | otherwise = "Abajo"
 where choqueIzq = x1+b1 - x2
       choqueDch = x2+b2 - x1
       choqueArr = y2+h2 - y1
       choqueAbj = y1+h1 - y2
       solapaMinimo = minimum [choqueIzq, choqueDch, choqueArr, choqueAbj]

--------------------------------------------

-- UTILIDADES DE LISTAS Y CADENAS

-- Función: splitOn
-- Divide una cadena en trozos cada vez que aparece un carácter separador dado
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs


-- Función: trim
-- Elimina los espacios en blanco (espacios, tabuladores, saltos de línea)
-- al principio y al final de una cadena
trim :: String -> String
trim = reverse. limpiaYPara. reverse. limpiaYPara
 where esBlanco :: Char -> Bool
       esBlanco x = x == ' ' || x == '\t' || x == '\n' 
       limpiaYPara :: String -> String
       limpiaYPara [] = []
       limpiaYPara original@(x:xs)
        | esBlanco x = limpiaYPara xs
        | otherwise = original


-- Función: contarSiCumple
-- Cuenta cuántos elementos de una lista cumplen una condición dada
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]


-- Función: list2Vector2
-- Convierte una lista de dos (o más) números en un vector/punto 2D;
-- lanza un error si la lista no tiene al menos dos elementos
list2Vector2 :: [Double] -> Vector2
list2Vector2 (x:y:_) = (x, y)
list2Vector2 [_]     = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 []      = error "Lista vacia, no es posible convertir en Vector2"

--------------------------------------------

-- PARSEO DEL NIVEL

-- Función: parsearNivel
-- Convierte la lista de líneas de texto leídas de un fichero de nivel 
-- en la estructura de datos que representa el nivel completo
parsearNivel :: [String] -> Grid
parsearNivel = id


-- Función: esSolido
-- Indica si una celda es una plataforma sólida
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False


-- Función: esMeta
-- Indica si una celda es la meta del nivel
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False


-- Función: esVacio
-- Indica si una celda está vacía (no hay nada en ella)
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False


-- Función: esEnemigo
-- Indica si una celda marca el punto de inicio de un enemigo
-- (cualquier carácter que no sea sólido, meta ni vacío)
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esVacio c || esMeta c)


-- Función: posicionesMeta
-- Dado el nivel completo, devuelve la lista de posiciones (fila, columna)
-- en las que aparece la meta
posicionesMeta :: Grid -> [Posicion]
posicionesMeta grid =
  [ (r, c) | (r, fila) <- zip [0..] grid
           , (c, celda) <- zip [0..] fila
           , esMeta celda ]


-- Función: posicionesEnemigos
-- Dado el nivel completo, devuelve la lista de posiciones y el identificador
-- de cada enemigo (fila, columna, identificador)
posicionesEnemigos :: Grid -> [Enemigo]
posicionesEnemigos grid =
  [ (r, c, celda) | (r, fila) <- zip [0..] grid
                  , (c, celda) <- zip [0..] fila
                  , esEnemigo celda ]


-- Función: agruparRachas
-- Recorre una fila del nivel y agrupa las columnas '#' consecutivas
-- en pares (columna de inicio, longitud de la racha)
agruparRachas :: Fila -> [(Int, Int)]                    
agruparRachas fila = medirRacha 0 fila 
  where
    medirRacha _ "" = []      
    medirRacha indice cadena@(c:cs)
        | esSolido c = (indice, length racha) : medirRacha (indice + length racha) resto 
        | otherwise  = medirRacha (indice + 1) cs
      where 
        (racha, resto) = span esSolido cadena

--------------------------------------------

-- PROPIEDADES CON QUICKCHECK

-- La suma de vectores es conmutativa
prop_suma_conmutativa :: Vector2 -> Vector2 -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.

-- La suma de vectores es asociativa
prop_suma_asociativa :: Vector2 -> Vector2 -> Vector2 -> Bool
prop_suma_asociativa a b c = sumaVectores suma1 c == sumaVectores a suma2
 where suma1 = sumaVectores a b
       suma2 = sumaVectores b c

-- *Main> quickCheck prop_suma_asociativa
-- *** Failed! Falsified (after 5 tests and 9 shrinks):
-- (0.0,1.0)
-- (0.0,1.0)
-- (0.0,0.32)
-- La propiedad es cierta matemáticamente, sin embargo da por mala
-- esta comprobación por errores de redondeo en el tipo Double.

-- La distancia siempre es no negativa
prop_distancia_no_negativa :: Vector2 -> Vector2 -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- *Main> quickCheck prop_distancia_no_negativa
-- +++ OK, passed 100 tests.

-- La distancia es simétrica
prop_distancia_simetrica :: Vector2 -> Vector2 -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- *Main> quickCheck prop_distancia_simetrica
-- +++ OK, passed 100 tests.

-- El solapamiento es simétrico
prop_solapan_simetrica :: Caja -> Caja -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- *Main> quickCheck prop_solapan_simetrica
-- +++ OK, passed 100 tests.

-- Trim es idempotente
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = (trim. trim) s == trim s

-- *Main> quickCheck prop_trim_idempotente
-- +++ OK, passed 100 tests.

-- Si no hay caracter separador, splitOn es la identidad
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador x xs = notElem x xs ==> splitOn x xs == [xs]

-- *Main> quickCheck prop_splitOn_sin_separador
-- +++ OK, passed 100 tests; 16 discarded.

-- contarSiCumple nunca es mayor que la longitud de la lista
-- En vez de usar un tipo 'a' genérico, concretaremos con Int para que genere los Test
-- En vez de usar una función a -> Bool genérica, usaremos la funcion odd.
prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado ls = contarSiCumple odd ls <= length ls

-- *Main> quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.

---------------------------------------------
