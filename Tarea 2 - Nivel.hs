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
 where choqueIzq = x1+b2 - x2
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

--			TAREA 2

---------------------------------------------


-------------------------------------------------------------------
--			          BLOQUE 1. RECURSIÓN
-------------------------------------------------------------------

--	1. Reimplementación recursiva de las consultas del NIVEL

-- posicionesMetaRec: recibe una lista de Strings y devuelve la posicion 
--			         (fila, columna) en la que se encuentran las Metas
posicionesMetaRec :: Grid -> [Posicion]
posicionesMetaRec = recorreFilasMeta 0

-- recorreFilasMeta: función auxiliar para recorrer recursivamente las Filas del nivel.
recorreFilasMeta :: Int -> Grid -> [Posicion]
recorreFilasMeta _ []     = []
recorreFilasMeta i (f:fs) = 
   recorreCeldasMeta i 0 f ++ recorreFilasMeta (i+1) fs

-- recorreCeldasMeta: función auxiliar para detectar recursivamente las celdas Meta.
recorreCeldasMeta :: Int -> Int -> Fila -> [Posicion]
recorreCeldasMeta _ _ [] = []
recorreCeldasMeta i j (c:cs)
    | esMeta c  = (i, j) : recorreCeldasMeta i (j+1) cs
    | otherwise = recorreCeldasMeta i (j+1) cs

--------

-- posicionesEnemigosRec:  recibe una lista de Strings y devuelve una lista de tuplas 
--						  (fila, columna, identificador) en la que se encuentran los enemigos
posicionesEnemigosRec :: Grid -> [Enemigo]
posicionesEnemigosRec = recorreFilasEnemigo 0

---- recorreFilasEnemigo: función auxiliar para recorrer recursivamente las Filas del nivel.
recorreFilasEnemigo :: Int -> Grid -> [Enemigo]
recorreFilasEnemigo _ []     = []
recorreFilasEnemigo i (f:fs) = 
   recorreCeldasEnemigo i 0 f ++ recorreFilasEnemigo (i+1) fs

-- recorreCeldasEnemigo: función auxiliar para detectar recursivamente las celdas Enemigo.
recorreCeldasEnemigo :: Int -> Int -> Fila -> [Enemigo]
recorreCeldasEnemigo _ _ [] = []
recorreCeldasEnemigo i j (c:cs)
    | esEnemigo c = (i, j, c) : recorreCeldasEnemigo i (j+1) cs
    | otherwise   = recorreCeldasEnemigo i (j+1) cs


--	2. Generalización por recursión de agruparRachas

-- agruparPorCaracter: recibe una lista de caracteres, devuelve un listado de las rachas
--                     de caractéres consecutivos, (inicio, longitud, carácter).
agruparPorCaracter :: Fila -> [(Int, Int, Celda)]
agruparPorCaracter "" = []
agruparPorCaracter (c:cs) = agruparAuxiliar 1 1 c cs -- leemos la primera celda en la llamada inicial

-- agruparAuxiliar: función auxiliar para recorrer recursivamente la Fila, con tres acumuladores:
--					Nº de celda actual, longitud de la racha actual, caracter de la racha actual.
agruparAuxiliar :: Int -> Int -> Celda -> Fila -> [(Int, Int, Celda)]

-- Caso Base: final de la fila. Añadimos la última racha.
agruparAuxiliar actual long c []       = [(actual-long, long, c)]

-- Caso General: comparamos celda anterior y celda actual.
agruparAuxiliar actual long c (f:fs)
 | c == f    = agruparAuxiliar (actual+1) (long+1) f fs
 | otherwise = racha : agruparAuxiliar (actual+1) 1 f fs
   where 
        racha = (actual-long, long, c)


--	3. Recursión numérica: simulación de movimiento


-- Estado = (Posicion, Velocidad)
type Estado = (Double, Double)

-- (Voy a considerar que la gravedad es un número real)
-- simularCaidaLibre: dados un numero de pasos (n), gravedad y estado actual, 
--                    devuelve el recorrido de los estados tras n pasos de caida.
simularCaidaLibre :: Int -> Double -> Estado -> [Estado]
simularCaidaLibre 0 _ e       = [e]
simularCaidaLibre n g e@(p,v) = e: simularCaidaLibre (n-1) g nuevoEstado
 where 
       nuevoEstado = (nuevaPos, nuevaVel)
       nuevaPos    = p + v
       nuevaVel    = v + g


--	4. Recursión sobre dos listas a la vez

-- zipPropio: empareja los elementos de las dos listas de entrada en una lista de tuplas.
zipPropio :: [a] -> [b] -> [(a,b)]
zipPropio [] _          = []
zipPropio _ []          = []
zipPropio (x:xs) (y:ys) = (x,y) : zipPropio xs ys 


-- 5. Recursion con descarte condicional: preparación de líneas

-- esComentarioOVacia: detecta si una cadena es comentario (comienza por ;)
--					   o bien está vacía.
esComentarioOVacia :: String -> Bool
esComentarioOVacia "" = True
esComentarioOVacia s@(x:xs)
    | x == ';'  = True
    | otherwise = soloBlancos s
 where soloBlancos :: String -> Bool
       soloBlancos "" = True
       soloBlancos (v:vs) 
           | v == ' '  = soloBlancos vs
           | otherwise = False

-- prepararLineas: recorta espacios en las cadenas introducidas y descarta aquellas que sean comentarios o estén vacías.
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (x:xs)
 | esComentarioOVacia limpia = prepararLineas xs
 | otherwise                 = limpia : prepararLineas xs
 where 
      limpia = trim x


-------------------------------------------------------------------
--			  BLOQUE 2. FUNCIONES DE ORDEN SUPERIOR
-------------------------------------------------------------------


-- mapPropio y filterPropio

-- mapPropio: aplica una función a elementos de una lista, uno a uno.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ []     = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- filterPropio: descarta los elementos de una lista que no cumplan una condición dada.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio p (x:xs)
    | p x       = x:filterPropio p xs
    | otherwise = filterPropio p xs


-- foldrPropio y foldlPropio

-- foldrPropio: combina los elementos de una lista de derecha a izquierda usando una función binaria y un caso base.
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
-- op(actual, retornoRecursivo) -> casoBase -> listaEntrada
foldrPropio _ c []     = c
foldrPropio f c (x:xs) = f x (foldrPropio f c xs)

-- foldlpropio: combina los elementos de una lista de izquierda a derecha, arrastrando un acumulador que empieza en un valor inicial.
foldlPropio :: (a -> b -> a) -> a -> [b] -> a
-- op(acum, actual) -> acum0 -> listaEntrada
foldlPropio _ acum []     = acum
foldlPropio f acum (x:xs) = foldlPropio f (f acum x) xs


-- 7. Aplicación justificada de foldr/foldl


-- hayColisionConAlguna: detecta si una caja colisiona con alguna de las introducidas.
{- 
   El uso de foldr permite establecer una recursión no terminal donde 
   se aprovecha la evaluación perezosa de (||). Con esto se consigue parar
   la ejecución en el momento que se cumpla alguna colision y nos ahorramos 
   tener que comprobar la colisión con toda la lista de cajas.
-}
hayColisionConAlguna :: Caja -> [Caja] -> Bool
hayColisionConAlguna x ys = foldr opColision False ys
  where opColision :: Caja -> Bool -> Bool
        opColision actual retornoRec = solapan x actual || retornoRec


-- contarCeldasQueCumplen: cuenta el numero de celdas que cumplen una condición en un nivel.
{-
   El uso de foldl conlleva un acumulador en el que podemos ir almacenando el conteo 
   conforme se va recorriendo la lista. Es óptimo porque permite obtener el resultado
   deseado recorriendo el Grid una única vez.
-}
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen p = foldl sumarFila 0
  where 
    -- Incrementa el acumulador con las celdas que cumplen la condición en la fila actual
    sumarFila :: Int -> Fila -> Int
    sumarFila ac filaActual = ac + contarCeldas filaActual
    -- Cuenta cuantas celdas de una fila cumplen la condición
    contarCeldas :: Fila -> Int
    contarCeldas = foldl sumarSiCumple 0
    -- Si una celda cumple la condición, incrementa el acumulador
    sumarSiCumple :: Int -> Celda -> Int
    sumarSiCumple acum actual 
        | p actual   = acum + 1
        | otherwise  = acum