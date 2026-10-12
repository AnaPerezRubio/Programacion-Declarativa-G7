-- Tarea 1 - Tommaso Saccenti

-- import de quickcheck
import Test.QuickCheck
import Data.Fixed (HasResolution(resolution))

-- tipos sinonimos

type Vector2 = (Double, Double) -- (x del vector, y del vector)
type Caja = (Double, Double, Double, Double) -- (x,y,ancho,alto)
type Celda = Char
type Grid = [String]

-- funciones

-- Funcion que suma dos vectores
sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Funcion que multiplica un vector por un numero
escalarVector :: Double -> Vector2 -> Vector2
escalarVector k (x, y) = (k * x, k * y)

-- Funcion que calcula la distancia euclidiana entre dos puntos
distancia :: Vector2 -> Vector2 -> Double
distancia (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
  where
    dx = x2 - x1
    dy = y2 - y1

-- Funcion que dice si dos cajas se superponen
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, anc1, alt1) (x2, y2, anc2, alt2) = solapanX && solapanY
  where
    solapanX = x1 < x2 + anc2 && x1 + anc1 > x2
    solapanY = y1 < y2 + alt2 && y1 + alt1 > y2

-- Funcion que calcula da que lado una caja ha solapado a un otra
ladoColision :: Caja -> Caja -> String
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
  | solapeArriba == minimo = "Arriba"
  | solapeAbajo  == minimo = "Abajo"
  | solapeIzq    == minimo = "Izquierda"
  | solapeDer    == minimo = "Derecha"
  where
    solapeArriba = (y2 + h2) - y1
    solapeAbajo  = (y1 + h1) - y2
    solapeIzq    = (x1 + w1) - x2
    solapeDer    = (x2 + w2) - x1
    minimo       = minimum [solapeArriba, solapeAbajo, solapeIzq, solapeDer]

-- Funcion que conta cuantos elementos de una lista cumplen una condicion
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple cond xs = length [x | x <- xs, cond x]

-- Funcion que convierte una lista de dos o mas elementos en un vector, lanzando un error si la lista tiene menos de dos elementos
list2Vector2 :: [Double] -> Vector2
list2Vector2 [] = error "No es posible crear un vector con una lista vacia"
list2Vector2 [_] = error "No es posible crear un vector con un solo elemento"
list2Vector2 (x:y:_) = (x, y)

-- Funcion que elimina los espacios blancos de una String
trim :: String -> String
trim str = quitaInicio (reverse (quitaInicio (reverse str)))
  where
    quitaInicio :: String -> String
    quitaInicio [] = []
    quitaInicio (x:xs)
        | esBlanco x = quitaInicio xs
        | otherwise  = x:xs
    esBlanco :: Char -> Bool
    esBlanco x = x == ' ' || x == '\t' || x == '\n'

-- Funcion que divide una String en una lista de Strings donde hay un caracter que se pasa come parametro
splitOn :: Char -> String -> [String]
splitOn carSep str = corta str ""
    where
        corta :: String -> String -> [String]
        corta [] actual = [actual]
        corta (x:xs) actual
            | x == carSep = actual : corta xs ""
            | otherwise = corta xs (actual ++ [x])

-- Funcion que convierte una lista de String in Grid (tipo sinonimo)
parsearNivel :: [String] -> Grid
parsearNivel nivel = nivel

-- Funcion que verifica si un caracter es solido (siguendo la structura del nivel)
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False

-- Funcion que verifica si un caracter es meta (siguendo la structura del nivel)
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False

-- Funcion que verifica si un caracter es vacio (siguendo la structura del nivel)
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False

-- Funcion que verifica si un caracter es un enemigo
esEnemigo :: Celda -> Bool
esEnemigo car = not (esSolido car) && not (esMeta car) && not (esVacio car)

-- Funcion que devuelve la posicion de la meta (M)
posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta nivel =
    [ (fila, columna) | (fila,linea) <- zip [0..] nivel, (columna, celda) <- zip [0..] linea, esMeta celda]

-- Funcion que devuelve la posicion de enemigos 
posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos nivel =
    [ (fila, columna,celda) | (fila,linea) <- zip [0..] nivel, (columna, celda) <- zip [0..] linea, esEnemigo celda]

-- Funcion que devuelve a que posicion de la lista aparece el # y cuantos estan
agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = fAuxiliaria 0 fila
    where
        fAuxiliaria :: Int -> String -> [(Int,Int)]
        fAuxiliaria _ [] = []
        fAuxiliaria pos xs =
            let (antes, resto) = span (/= '#') xs
                (rachas, despues) = span (== '#') resto
            in if null rachas
                then []
                else (pos + length antes, length rachas) : fAuxiliaria (pos + length antes + length rachas) despues

-- FUNCIONES DE TEST POR QUICKTEST

-- Funcion que escala un vector por 1 y ve si el vector no cambia
prop_escalar_neutro :: Vector2 -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- Funcion que verifica si la distancia entre dos puntos nunca es negativa
prop_distancia_no_negativa :: Vector2 -> Vector2 -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- Funcion que verifica si sumar un vector con el vector nulo no lo cambia
prop_suma_neutro :: Vector2 -> Bool
prop_suma_neutro v = sumaVectores v (0,0) == v

-- Funcion que verifica si la suma de los vectores es conmutativa
prop_suma_conmutativa :: Vector2 -> Vector2 -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- Funcion que verifica si la distancia de dos puntos es simetrica
prop_distancia_simetrica :: Vector2 -> Vector2 -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- Funcion que verifica si usar trim dos veces es la misma cosa que usarla una sola vez 
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

-- Tarea 2

-- Funcion que devuelve (fila, columna) de la meta mediante recursion
posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec grid = recorrerFilas 0 grid
    where
        recorrerFilas :: Int -> Grid -> [(Int, Int)]
        recorrerFilas _ [] = []
        recorrerFilas x (linea:resto) = recorrerClmn x 0 linea ++ recorrerFilas (x + 1) resto

        recorrerClmn :: Int -> Int -> String -> [(Int, Int)]
        recorrerClmn _ _ [] = []
        recorrerClmn x y (celda:resto)
            | esMeta celda = (x,y) : recorrerClmn x (y + 1) resto
            | otherwise = recorrerClmn x (y + 1) resto


-- Funcion que devuelve (fila, columna, caracter) de enemigos mediante recursion
posicionesEnemigosRec :: Grid -> [(Int, Int, Char)]
posicionesEnemigosRec grid = recorrerFilas 0 grid 
    where
        recorrerFilas :: Int -> Grid -> [(Int,Int,Char)]
        recorrerFilas _ [] = []
        recorrerFilas x (linea:resto) = recorrerClmn x 0 linea ++ recorrerFilas (x + 1) resto

        recorrerClmn :: Int -> Int -> String -> [(Int,Int,Char)]
        recorrerClmn _ _ [] = []
        recorrerClmn x y (celda:resto)
            | esEnemigo celda = (x, y, celda) : recorrerClmn x (y + 1) resto
            | otherwise = recorrerClmn x (y + 1) resto

-- Funcion que recorre una fila y agrupa en trios (y, longitud, caracter) las rachas consecutivas
agruparPorCaracter :: String -> [(Int, Int, Char)]
agruparPorCaracter fila = aux 0 fila
  where
    aux :: Int -> String -> [(Int, Int, Char)]
    aux _ [] = []
    aux y (c:cs) = (y, len, c) : aux (y + len) resto
      where
        (len, resto) = contarIguales 1 c cs

    contarIguales :: Int -> Char -> String -> (Int, String)
    contarIguales n _ [] = (n, [])
    contarIguales n c (x:xs)
      | c == x    = contarIguales (n + 1) c xs
      | otherwise = (n, x:xs)

-- Funcion que calcula la lista de estados (posición x, velocidad v) en caída libre
simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)]
simularCaidaLibre n g estadoInicial
  | n <= 0    = [estadoInicial]
  | otherwise = estadoInicial : simularCaidaLibre (n - 1) g (pasoCaida estadoInicial)
  where
    -- Funcion auxiliar  para calcular el estado siguiente tras un paso de simulacion
    pasoCaida :: (Double, Double) -> (Double, Double)
    pasoCaida (x, v) = (x + v, v + g)

-- Funcion que empareja los elementos de dos listas hasta que una de las dos se agota
zipPropio :: [a] -> [b] -> [(a, b)]
zipPropio [] _          = []
zipPropio _ []          = []
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys

 -- Funcion que comprueba si una linea recortada es vacia o empieza por comentario
esComentarioOVacia :: String -> Bool
esComentarioOVacia s = null s' || head s' == ';'
  where
    s' = trim s

-- Funcion que limpia una lista de lineas recortando espacios y descartando lineas vacias o comentarios
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (linea:resto)
  | esComentarioOVacia lineaLimpia = prepararLineas resto
  | otherwise                      = lineaLimpia : prepararLineas resto
  where
    lineaLimpia = trim linea

-- Funcion que aplica una funcion a cada elemento de una lista manteniendo el orden
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ []     = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Funcion que filtra los elementos de una lista que cumplen una condicion dada
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ []     = []
filterPropio p (x:xs)
  | p x       = x : filterPropio p xs
  | otherwise = filterPropio p xs

-- Funcion que plega por la derecha: combina elementos de derecha a izquierda
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ v []     = v
foldrPropio f v (x:xs) = f x (foldrPropio f v xs)

-- Funcion que plega por la izquierda: combina elementos de izquierda a derecha acumulando el resultado
foldlPropio :: (a -> b -> a) -> a -> [b] -> a
foldlPropio _ v []     = v
foldlPropio f v (x:xs) = foldlPropio f (f v x) xs

-- Se utiliza foldr porque el operador lógico (||) permite la evaluación perezosa. Como se detecta una colision con algun obstaculo foldr devuelve True, sin necesidad de evaluar el resto de la lista
hayColisionConAlguna :: Caja -> [Caja] -> Bool
hayColisionConAlguna cajaJugador obstaculos = foldr colisionaConCaja False obstaculos
  where
    colisionaConCaja obs acc = solapan cajaJugador obs || acc

-- Se utiliza foldl porque necesitamos contar y recorrer todas las celdas del nivel, ya que no se puede terminar antes de tiempo
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen cond grid = foldl contarFila 0 grid
  where
    contarFila acc fila = foldl contarCelda acc fila
    contarCelda acc' celda
      | cond celda = acc' + 1
      | otherwise  = acc'

-- FUNCIONES DE TEST POR QUICKTEST

-- Funcion que comprueba si la funcion mapPropio equivale a la funcion map normal
prop_mapPropio_equivale_map :: (Int -> Int) -> [Int] -> Bool
prop_mapPropio_equivale_map f xs = mapPropio f xs == map f xs

-- Funcion que comprueba si la funcion filterPropio equivale a la funcion filter normal
prop_filterPropio_equivale_filter :: (Int -> Bool) -> [Int] -> Bool
prop_filterPropio_equivale_filter p xs = filterPropio p xs == filter p xs

-- Funcion que comprueba si la funcion foldrPropio equivale a la funcion foldr normal
prop_foldrPropio_equivale_foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldrPropio_equivale_foldr f v xs = foldrPropio f v xs == foldr f v xs

-- Funcion que comprueba si la funcion foldlPropio equivale a la funcion foldl normal
prop_foldlPropio_equivale_foldl :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldlPropio_equivale_foldl f v xs = foldlPropio f v xs == foldl f v xs

-- Funcion que comprueba si la funcion zipProprio equivale a la funcion zip normal
prop_zipPropio_equivale_zip :: [Int] -> [String] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys

-- Funcion que comprueba si la simulacion de caida libre genera exactamente n + 1 estados para n pasos.
prop_simularCaidaLibre_longitud :: Int -> Double -> (Double, Double) -> Property
prop_simularCaidaLibre_longitud n g estado =
  n >= 0 ==> length (simularCaidaLibre n g estado) == n + 1

-- Funcion que comprueba si no existe colision cuando la lista de obstaculos esta vacia.
prop_hayColisionConAlguna_lista_vacia :: Caja -> Bool
prop_hayColisionConAlguna_lista_vacia caja = not (hayColisionConAlguna caja [])