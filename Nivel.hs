import Test.QuickCheck
-- TAREA 1
-- TIPOS SINÓNIMOS

type Vector2 = (Double, Double)
type Caja = (Double, Double, Double, Double)
type Lado = String
type Celda = Char
type Nivel = [[Celda]]

-- 1. VECTORES 2D

-- Suma componente a componente dos vectores/puntos 2D.
sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Double -> Vector2 -> Vector2
escalarVector k (x, y) = (k * x, k * y)

-- Calcula la distancia euclídea entre dos puntos 2D.
distancia :: Vector2 -> Vector2 -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- 2. CAJAS DE COLISIÓN

-- Indica si dos cajas delimitadoras alineadas con los ejes se solapan.
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
    (x1 < x2 + w2) && (x2 < x1 + w1) && (y1 < y2 + h2) && (y2 < y1 + h1)

-- 3. LADO DE COLISIÓN

-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto.
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
    | minSolape == solapeArriba = "Arriba"
    | minSolape == solapeAbajo  = "Abajo"
    | minSolape == solapeIzq    = "Izquierda"
    | otherwise                 = "Derecha"
  where
    solapeArriba = (y2 + h2) - y1
    solapeAbajo  = (y1 + h1) - y2
    solapeIzq    = (x1 + w1) - x2
    solapeDer    = (x2 + w2) - x1
    minSolape    = minimum [solapeArriba, solapeAbajo, solapeIzq, solapeDer]

-- 4. UTILIDADES DE LISTAS Y CADENAS

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
trim s = reverse (quitaEspacios (reverse (quitaEspacios s)))
  where
    quitaEspacios :: String -> String
    quitaEspacios [] = []
    quitaEspacios (x:xs)
        | x == ' ' || x == '\t' || x == '\n' = quitaEspacios xs
        | otherwise = x:xs

-- Cuenta cuántos elementos de una lista cumplen una condición dada.
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple cond xs = length [x | x <- xs, cond x]

-- Convierte una lista de dos (o más) números en un vector 2D.
list2Vector2 :: [Double] -> Vector2
list2Vector2 []      = error "Lista vacia no es posible convertir en Vector2"
list2Vector2 [_]     = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x, y)

-- 5. PARSEO DEL NIVEL

-- Convierte la lista de líneas de texto en la estructura de datos del nivel.
parsearNivel :: [String] -> Nivel
parsearNivel lineas = lineas

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

-- Devuelve la lista de posiciones (fila, columna) de la meta.
posicionesMeta :: Nivel -> [(Int, Int)]
posicionesMeta nivel =
    [ (f, c) | (f, fila) <- zip [0..] nivel, (c, celda) <- zip [0..] fila, esMeta celda ]

-- Devuelve la lista de posiciones e identificadores de cada enemigo.
posicionesEnemigos :: Nivel -> [(Int, Int, String)]
posicionesEnemigos nivel =
    [ (f, c, [celda]) | (f, fila) <- zip [0..] nivel, (c, celda) <- zip [0..] fila, esEnemigo celda ]

-- Agrupa columnas '#' consecutivas en pares (columna de inicio, longitud).
agruparRachas :: [Celda] -> [(Int, Int)]
agruparRachas fila = buscarRachas 0 0 fila
  where
    buscarRachas :: Int -> Int -> [Celda] -> [(Int, Int)]
    buscarRachas _ 0 [] = []
    buscarRachas inicio long [] = [(inicio, long)]
    buscarRachas inicio 0 (c:cs)
        | esSolido c = buscarRachas inicio 1 cs
        | otherwise  = buscarRachas (inicio + 1) 0 cs
    buscarRachas inicio long (c:cs)
        | esSolido c = buscarRachas inicio (long + 1) cs
        | otherwise  = (inicio, long) : buscarRachas (inicio + long + 1) 0 cs

-- 6. BONUS: PROPIEDADES CON QUICKCHECK (TAREA 1)

-- Comprueba que la suma de vectores es conmutativa.
prop_suma_conmutativa :: Vector2 -> Vector2 -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- Comprueba que la suma de vectores es asociativa.
prop_suma_asociativa :: Vector2 -> Vector2 -> Vector2 -> Bool
prop_suma_asociativa a b c = sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)

-- Comprueba que escalar por 1 no modifica el vector.
prop_escalar_neutro :: Vector2 -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- Comprueba que la distancia entre dos puntos nunca es negativa.
prop_distancia_no_negativa :: Vector2 -> Vector2 -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- Comprueba que la distancia es simétrica.
prop_distancia_simetrica :: Vector2 -> Vector2 -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- Comprueba que la relación de solapamiento es simétrica.
prop_solapan_simetrica :: Caja -> Caja -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- Comprueba que trim es idempotente.
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- Comprueba que splitOn devuelve un único elemento si no aparece el separador.
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador c s = not (c `elem` s) ==> splitOn c s == [s]

-- Comprueba que contarSiCumple nunca supera la longitud de la lista.
prop_contarSiCumple_acotado :: Char -> String -> Bool
prop_contarSiCumple_acotado c s = contarSiCumple (== c) s <= length s

-- TAREA 2: BLOQUE 1 - RECURSIÓN

-- Devuelve las posiciones de la meta usando recursión explícita.
posicionesMetaRec :: Nivel -> [(Int, Int)]
posicionesMetaRec nivel = recorrerFilas 0 nivel
  where
    recorrerFilas :: Int -> Nivel -> [(Int, Int)]
    recorrerFilas _ [] = []
    recorrerFilas f (fila:filas) = recorrerColumnas f 0 fila ++ recorrerFilas (f + 1) filas

    recorrerColumnas :: Int -> Int -> [Celda] -> [(Int, Int)]
    recorrerColumnas _ _ [] = []
    recorrerColumnas f c (celda:celdas)
        | esMeta celda = (f, c) : recorrerColumnas f (c + 1) celdas
        | otherwise    = recorrerColumnas f (c + 1) celdas

-- Devuelve las posiciones e identificadores de los enemigos usando recursión explícita.
posicionesEnemigosRec :: Nivel -> [(Int, Int, String)]
posicionesEnemigosRec nivel = recorrerFilas 0 nivel
  where
    recorrerFilas :: Int -> Nivel -> [(Int, Int, String)]
    recorrerFilas _ [] = []
    recorrerFilas f (fila:filas) = recorrerColumnas f 0 fila ++ recorrerFilas (f + 1) filas

    recorrerColumnas :: Int -> Int -> [Celda] -> [(Int, Int, String)]
    recorrerColumnas _ _ [] = []
    recorrerColumnas f c (celda:celdas)
        | esEnemigo celda = (f, c, [celda]) : recorrerColumnas f (c + 1) celdas
        | otherwise       = recorrerColumnas f (c + 1) celdas

-- Agrupa rachas de caracteres consecutivos iguales en tríos (inicio, longitud, carácter).
agruparPorCaracter :: String -> [(Int, Int, Char)]
agruparPorCaracter fila = agruparAux 0 fila
  where
    agruparAux :: Int -> String -> [(Int, Int, Char)]
    agruparAux _ [] = []
    agruparAux inicio (c:cs) = (inicio, long, c) : agruparAux (inicio + long) resto
      where
        (long, resto) = contarIguales 1 c cs

        contarIguales :: Int -> Char -> String -> (Int, String)
        contarIguales n _ [] = (n, [])
        contarIguales n chr (y:ys)
            | chr == y  = contarIguales (n + 1) chr ys
            | otherwise = (n, y:ys)

-- Calcula recursivamente los estados de un objeto en caída libre.
simularCaidaLibre :: Int -> Double -> Vector2 -> [Vector2]
simularCaidaLibre 0 _ estado = [estado]
simularCaidaLibre n g estado = estado : simularCaidaLibre (n - 1) g (siguientePaso g estado)
  where
    siguientePaso :: Double -> Vector2 -> Vector2
    siguientePaso gravedad (pos, vel) = (pos + vel, vel + gravedad)

-- Empareja los elementos de dos listas hasta que una se acaba.
zipPropio :: [a] -> [b] -> [(a, b)]
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys

-- Indica si una línea está vacía o es un comentario que empieza por ';'.
esComentarioOVacia :: String -> Bool
esComentarioOVacia [] = True
esComentarioOVacia (';':_) = True
esComentarioOVacia _ = False

-- Limpia espacios, descarta líneas vacías y comentarios.
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (l:ls)
    | esComentarioOVacia lineaLimpia = prepararLineas ls
    | otherwise                      = lineaLimpia : prepararLineas ls
  where
    lineaLimpia = trim l

-- TAREA 2: BLOQUE 2 - FUNCIONES DE ORDEN SUPERIOR

-- Aplica una función a cada elemento de una lista.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ [] = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Se queda con los elementos que cumplen una condición.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio p (x:xs)
    | p x       = x : filterPropio p xs
    | otherwise = filterPropio p xs

-- Combina elementos de derecha a izquierda con una función binaria y un valor inicial.
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ initVal [] = initVal
foldrPropio f initVal (x:xs) = f x (foldrPropio f initVal xs)

-- Combina elementos de izquierda a derecha arrastrando un acumulador.
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ acc [] = acc
foldlPropio f acc (x:xs) = foldlPropio f (f acc x) xs

-- Indica si la caja colisiona con alguna de la lista de obstáculos.
-- Se usa foldr porque (||) cortocircuita y puede detenerse en la primera colisión.
hayColisionConAlguna :: Caja -> [Caja] -> Bool
hayColisionConAlguna caja obstaculos = foldr compruebaColision False obstaculos
  where
    compruebaColision :: Caja -> Bool -> Bool
    compruebaColision obs acc = solapan caja obs || acc

-- Cuenta cuántas celdas del nivel cumplen una condición.
-- Se usa foldl porque es una acumulación numérica estricta sin cortocircuito.
contarCeldasQueCumplen :: (Celda -> Bool) -> Nivel -> Int
contarCeldasQueCumplen condicion nivel = foldl sumarFilas 0 nivel
  where
    sumarFilas :: Int -> [Celda] -> Int
    sumarFilas acc fila = acc + foldl contarEnFila 0 fila

    contarEnFila :: Int -> Celda -> Int
    contarEnFila acc celda
        | condicion celda = acc + 1
        | otherwise       = acc

-- BONUS QUICKCHECK (TAREA 2)

-- Comprueba que mapPropio equivale a map.
prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs = mapPropio (*2) xs == map (*2) xs

-- Comprueba que filterPropio equivale a filter.
prop_filterPropio_equivale_filter :: [Int] -> Bool
prop_filterPropio_equivale_filter xs = filterPropio even xs == filter even xs

-- Comprueba que foldrPropio equivale a foldr.
prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs = foldrPropio (+) 0 xs == foldr (+) 0 xs

-- Comprueba que foldlPropio equivale a foldl.
prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs = foldlPropio (+) 0 xs == foldl (+) 0 xs

-- Comprueba que zipPropio equivale a zip.
prop_zipPropio_equivale_zip :: [Int] -> [Char] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys

-- Comprueba que simularCaidaLibre produce n+1 estados para n >= 0.
prop_simularCaidaLibre_longitud :: Int -> Double -> Vector2 -> Property
prop_simularCaidaLibre_longitud n g v =
    n >= 0 ==> length (simularCaidaLibre n g v) == n + 1

-- Comprueba que no hay colisión contra una lista vacía de obstáculos.
prop_hayColisionConAlguna_lista_vacia :: Caja -> Bool
prop_hayColisionConAlguna_lista_vacia c = not (hayColisionConAlguna c [])