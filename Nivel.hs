-- =============================================================
-- Programación Declarativa - Tareas 1 y 2
-- Versión consolidada del Grupo 7
-- =============================================================

module Nivel where

import Test.QuickCheck

-- =============================================================
-- TIPOS SINÓNIMOS
-- =============================================================

type Vector2  = (Double, Double)
type Caja     = (Double, Double, Double, Double)   -- (x, y, ancho, alto)
type Lado     = String
type Celda    = Char
type Fila     = [Celda]
type Grid     = [Fila]
type Posicion = (Int, Int)
type Enemigo  = (Int, Int, Char)
type Estado   = (Double, Double)                   -- (posición, velocidad)

-- =============================================================
-- TAREA 1
-- =============================================================

-- -------------------------------------------------------------
-- 1. Vectores 2D
-- -------------------------------------------------------------

-- Suma componente a componente dos vectores/puntos 2D.
sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Double -> Vector2 -> Vector2
escalarVector k (x, y) = (k * x, k * y)

-- Calcula la distancia euclídea entre dos puntos 2D.
distancia :: Vector2 -> Vector2 -> Double
distancia (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1

-- -------------------------------------------------------------
-- 2. Cajas de colisión
-- -------------------------------------------------------------

-- Indica si dos cajas alineadas con los ejes se solapan.
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, anc1, alt1) (x2, y2, anc2, alt2) = solapanX && solapanY
  where
    solapanX = x1 < x2 + anc2 && x1 + anc1 > x2
    solapanY = y1 < y2 + alt2 && y1 + alt1 > y2

-- -------------------------------------------------------------
-- 3. Lado de colisión
-- -------------------------------------------------------------

-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto.
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
    | sArriba <= sAbajo && sArriba <= sIzq && sArriba <= sDer = "Arriba"
    | sAbajo  <= sIzq   && sAbajo  <= sDer                    = "Abajo"
    | sIzq    <= sDer                                         = "Izquierda"
    | otherwise                                               = "Derecha"
  where
    sArriba = (y1 + h1) - y2
    sAbajo  = (y2 + h2) - y1
    sIzq    = (x1 + w1) - x2
    sDer    = (x2 + w2) - x1

-- -------------------------------------------------------------
-- 4. Utilidades de listas y cadenas
-- -------------------------------------------------------------

-- Divide una cadena en trozos cada vez que aparece un carácter separador.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

-- Elimina los espacios en blanco al principio y al final de una cadena.
trim :: String -> String
trim = dropWhile esBlanco . reverse . dropWhile esBlanco . reverse
  where
    esBlanco c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- Cuenta cuántos elementos de una lista cumplen una condición dada.
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]

-- Convierte una lista de dos (o más) números en un vector/punto 2D.
list2Vector2 :: [Double] -> Vector2
list2Vector2 (x:y:_) = (x, y)
list2Vector2 [_]     = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 []      = error "Lista vacía, no es posible convertir en Vector2"

-- -------------------------------------------------------------
-- 5. Parseo del nivel
-- -------------------------------------------------------------

-- Convierte la lista de líneas de texto en la estructura de datos del nivel.
parsearNivel :: [String] -> Grid
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

-- Devuelve la lista de posiciones (fila, columna) donde aparece la meta.
posicionesMeta :: Grid -> [Posicion]
posicionesMeta grid =
    [ (r, c) | (r, fila) <- zip [0..] grid
             , (c, celda) <- zip [0..] fila
             , esMeta celda ]

-- Devuelve la lista de posiciones e identificadores de cada enemigo.
posicionesEnemigos :: Grid -> [Enemigo]
posicionesEnemigos grid =
    [ (r, c, celda) | (r, fila) <- zip [0..] grid
                    , (c, celda) <- zip [0..] fila
                    , esEnemigo celda ]

-- Agrupa columnas '#' consecutivas en pares (columna de inicio, longitud).
agruparRachas :: Fila -> [(Int, Int)]
agruparRachas fila = medirRacha 0 fila
  where
    medirRacha _ "" = []
    medirRacha indice cadena@(c:cs)
        | esSolido c = (indice, length racha) : medirRacha (indice + length racha) resto
        | otherwise  = medirRacha (indice + 1) cs
      where
        (racha, resto) = span esSolido cadena

-- =============================================================
-- TAREA 2 - BLOQUE 1: RECURSIÓN
-- =============================================================

-- -------------------------------------------------------------
-- 1. Reimplementación recursiva de las consultas del nivel
-- -------------------------------------------------------------

-- Devuelve, mediante recursión explícita, las posiciones (fila, columna) de las metas.
posicionesMetaRec :: Grid -> [Posicion]
posicionesMetaRec grid = recorrerFilas 0 grid
  where
    recorrerFilas :: Int -> Grid -> [Posicion]
    recorrerFilas _ []           = []
    recorrerFilas r (fila:filas) = recorrerColumnas r 0 fila ++ recorrerFilas (r + 1) filas

    recorrerColumnas :: Int -> Int -> Fila -> [Posicion]
    recorrerColumnas _ _ [] = []
    recorrerColumnas r c (celda:celdas)
        | esMeta celda = (r, c) : recorrerColumnas r (c + 1) celdas
        | otherwise    = recorrerColumnas r (c + 1) celdas

-- Devuelve, mediante recursión explícita, las posiciones (fila, columna, id) de los enemigos.
posicionesEnemigosRec :: Grid -> [Enemigo]
posicionesEnemigosRec grid = recorrerFilas 0 grid
  where
    recorrerFilas :: Int -> Grid -> [Enemigo]
    recorrerFilas _ []           = []
    recorrerFilas r (fila:filas) = recorrerColumnas r 0 fila ++ recorrerFilas (r + 1) filas

    recorrerColumnas :: Int -> Int -> Fila -> [Enemigo]
    recorrerColumnas _ _ [] = []
    recorrerColumnas r c (celda:celdas)
        | esEnemigo celda = (r, c, celda) : recorrerColumnas r (c + 1) celdas
        | otherwise       = recorrerColumnas r (c + 1) celdas

-- -------------------------------------------------------------
-- 2. Generalización por recursión de agruparRachas
-- -------------------------------------------------------------

-- Agrupa en tríos (columna de inicio, longitud, carácter) las rachas consecutivas iguales.
agruparPorCaracter :: Fila -> [(Int, Int, Celda)]
agruparPorCaracter fila = aux 0 fila
  where
    aux :: Int -> Fila -> [(Int, Int, Celda)]
    aux _ [] = []
    aux inicio (c:cs) = (inicio, long, c) : aux (inicio + long) resto
      where
        (long, resto) = contarIguales 1 c cs

    contarIguales :: Int -> Celda -> Fila -> (Int, Fila)
    contarIguales n _ [] = (n, [])
    contarIguales n chr (y:ys)
        | chr == y  = contarIguales (n + 1) chr ys
        | otherwise = (n, y:ys)

-- -------------------------------------------------------------
-- 3. Recursión numérica: simulación de movimiento
-- -------------------------------------------------------------

-- Simula la caída libre de un objeto devolviendo los n+1 estados por los que pasa.
simularCaidaLibre :: Int -> Double -> Estado -> [Estado]
simularCaidaLibre 0 _ e = [e]
simularCaidaLibre n g e@(p, v) = e : simularCaidaLibre (n - 1) g nuevoEstado
  where
    nuevoEstado = (p + v, v + g)

-- -------------------------------------------------------------
-- 4. Recursión sobre dos listas a la vez
-- -------------------------------------------------------------

-- Empareja uno a uno los elementos de dos listas, deteniéndose en la más corta.
zipPropio :: [a] -> [b] -> [(a, b)]
zipPropio [] _          = []
zipPropio _ []          = []
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys

-- -------------------------------------------------------------
-- 5. Recursión con descarte condicional: preparación de líneas
-- -------------------------------------------------------------

-- Indica si una línea está vacía o es un comentario que empieza por ';'.
esComentarioOVacia :: String -> Bool
esComentarioOVacia []      = True
esComentarioOVacia (';':_) = True
esComentarioOVacia _       = False

-- Limpia una lista de líneas, recortando espacios y descartando las vacías y comentarios.
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (l:ls)
    | esComentarioOVacia lineaLimpia = prepararLineas ls
    | otherwise                      = lineaLimpia : prepararLineas ls
  where
    lineaLimpia = trim l

-- =============================================================
-- TAREA 2 - BLOQUE 2: FUNCIONES DE ORDEN SUPERIOR
-- =============================================================

-- -------------------------------------------------------------
-- 5. mapPropio y filterPropio
-- -------------------------------------------------------------

-- Aplica una función a cada elemento de una lista, preservando el orden.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ []     = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Se queda con los elementos de una lista que cumplen una condición.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio p (x:xs)
    | p x       = x : filterPropio p xs
    | otherwise = filterPropio p xs

-- -------------------------------------------------------------
-- 6. foldrPropio y foldlPropio
-- -------------------------------------------------------------

-- Combina elementos de derecha a izquierda con una función binaria y un valor inicial.
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ z []     = z
foldrPropio f z (x:xs) = f x (foldrPropio f z xs)

-- Combina elementos de izquierda a derecha arrastrando un acumulador.
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ acc []     = acc
foldlPropio f acc (x:xs) = foldlPropio f (f acc x) xs

-- -------------------------------------------------------------
-- 7. Aplicación justificada de foldr/foldl
-- -------------------------------------------------------------

-- Comprueba si una caja colisiona con alguna de una lista de obstáculos.
-- Se usa foldr (no foldl) porque el operador (||) cortocircuita: gracias a
-- la evaluación perezosa, en cuanto se encuentra una colisión no se evalúa
-- el resto de la lista, lo cual es óptimo para listas largas de obstáculos.
hayColisionConAlguna :: Caja -> [Caja] -> Bool
hayColisionConAlguna caja obstaculos = foldr compruebaColision False obstaculos
  where
    compruebaColision :: Caja -> Bool -> Bool
    compruebaColision obstaculo acc = solapan caja obstaculo || acc

-- Cuenta cuántas celdas del nivel cumplen una condición dada.
-- Se usa foldl (no foldr) porque es una acumulación numérica simple que debe
-- recorrer todo el nivel, sin posibilidad de cortocircuitar; la asociación
-- por la izquierda encaja de forma natural con el acumulador.
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen p = foldl sumarFila 0
  where
    sumarFila :: Int -> Fila -> Int
    sumarFila ac fila = ac + contarEnFila fila

    contarEnFila :: Fila -> Int
    contarEnFila = foldl sumarSiCumple 0

    sumarSiCumple :: Int -> Celda -> Int
    sumarSiCumple ac celda
        | p celda   = ac + 1
        | otherwise = ac

-- =============================================================
-- PROPIEDADES QUICKCHECK (BONUS)
-- =============================================================

-- ---- Tarea 1 ----

-- La suma de vectores es conmutativa.
prop_suma_conmutativa :: Vector2 -> Vector2 -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- La suma de vectores es asociativa (con tolerancia por redondeo de Double).
prop_suma_asociativa :: Vector2 -> Vector2 -> Vector2 -> Bool
prop_suma_asociativa a b c =
    casiIgual (sumaVectores (sumaVectores a b) c)
              (sumaVectores a (sumaVectores b c))
  where
    casiIgual :: Vector2 -> Vector2 -> Bool
    casiIgual (x1, y1) (x2, y2) =
        abs (x1 - x2) < 1e-9 && abs (y1 - y2) < 1e-9

-- Escalar por 1 no modifica el vector.
prop_escalar_neutro :: Vector2 -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- La distancia entre dos puntos nunca es negativa.
prop_distancia_no_negativa :: Vector2 -> Vector2 -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- La distancia es simétrica.
prop_distancia_simetrica :: Vector2 -> Vector2 -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- La relación de solapamiento de cajas es simétrica.
prop_solapan_simetrica :: Caja -> Caja -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- trim es idempotente.
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- splitOn devuelve una única pieza si el separador no aparece.
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador c s = notElem c s ==> splitOn c s == [s]

-- contarSiCumple nunca supera la longitud de la lista.
prop_contarSiCumple_acotado :: Char -> String -> Bool
prop_contarSiCumple_acotado c s = contarSiCumple (== c) s <= length s

-- ---- Tarea 2 ----

-- mapPropio equivale a map.
prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs = mapPropio (*2) xs == map (*2) xs

-- filterPropio equivale a filter.
prop_filterPropio_equivale_filter :: [Int] -> Bool
prop_filterPropio_equivale_filter xs = filterPropio even xs == filter even xs

-- foldrPropio equivale a foldr.
prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs = foldrPropio (-) 0 xs == foldr (-) 0 xs

-- foldlPropio equivale a foldl.
prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs = foldlPropio (-) 0 xs == foldl (-) 0 xs

-- zipPropio equivale a zip.
prop_zipPropio_equivale_zip :: [Int] -> [Char] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys

-- simularCaidaLibre produce n+1 estados cuando n >= 0.
prop_simularCaidaLibre_longitud :: Int -> Double -> Estado -> Property
prop_simularCaidaLibre_longitud n g e =
    n >= 0 ==> length (simularCaidaLibre n g e) == n + 1

-- Nunca hay colisión contra una lista vacía de obstáculos.
prop_hayColisionConAlguna_lista_vacia :: Caja -> Bool
prop_hayColisionConAlguna_lista_vacia c = not (hayColisionConAlguna c [])