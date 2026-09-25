-- 1. VECTORES 2D 

import Test.QuickCheck 


-- Tipos sinónimos
type Punto = (Double, Double)

-- sumaVectores: calcula la suma de dos vectores componente a componente
sumaVectores :: Punto -> Punto -> Punto
sumaVectores x y = (fst x + fst y, snd x + snd y)

-- escalarVector: multiplica un vector por un escalar
escalarVector :: Double -> Punto -> Punto
escalarVector x p = (x*(fst p), x* (snd p))

-- distancia: calcula la distancia euclídea entre dos puntos
distancia :: Punto -> Punto -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- 2. CAJAS DE COLISIÓN

-- Tipo sinonimo
type Caja = (Double, Double, Double, Double)

-- Voy a considerar que la posición dará la esquina inferior izquierda.
-- Dos cajas NO se solapan (teniendo en cuenta los límites) si:
-- x2 >= x1 +b1 o
-- x2 + b2 <= x1 o
-- y2 >= y1 +h1
-- y2 + h2 <= y1
-- La condición de que SI se solapen se obtiene negando la anterior y aplicando leyes de morgan.
solapan :: Caja -> Caja -> Bool
solapan (x1,y1,b1,h1) (x2,y2,b2,h2) = (x2 < x1+b1) && (x2+b2 > x1) && (y2 < y1+h1) && (y2+h2 > y1)

-- 3. LADO DE COLISIÓN

-- ladoColision: dadas dos cajas que colisionan, 
-- devuelve el lado por el que se produce el contacto (el de menor solape)
ladoColision :: Caja -> Caja -> String
ladoColision (x1,y1,b1,h1) (x2,y2,b2,h2)
-- | not (solapan (x1,y1,b1,h1) (x2,y2,b2,h2)) = error ">> ladoColision: Las dos cajas introducidas no solapan."
 | choqueIzq == solapaMinimo = "Izquierda"
 | choqueDch == solapaMinimo = "Derecha"
 | choqueArr == solapaMinimo = "Arriba"
 | choqueAbj == solapaMinimo = "Abajo"
 where choqueIzq = x1+b2 - x2
       choqueDch = x2+b2 - x1
       choqueArr = y2+h2 - y1
       choqueAbj = y1+h1 - y2
       solapaMinimo = minimum [choqueIzq, choqueDch, choqueArr, choqueAbj]

-- 4. UTILIDADES DE LISTAS Y CADENAS

-- splitOn: divide una cadena en trozos cada vez que aparece un separador dado.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn s (x:xs) 
 | x /= s = (x:(head pasoRecursivo)):(tail pasoRecursivo)
 | x == s = "":pasoRecursivo
 where pasoRecursivo = splitOn s xs

-- trim: elimina espacios en blanco al principio y final de una cadena
trim :: String -> String
trim s = (reverse. limpiaYPara. reverse. limpiaYPara) s

-- limpiaYPara: función auxiliar que elimina espacios blanco hasta encontrar un carácter y para.
limpiaYPara :: String -> String
limpiaYPara [] = []
limpiaYPara (x:xs)
 | x == ' ' = limpiaYPara xs
 | x == '\t' = limpiaYPara xs
 | x == '\n' = limpiaYPara xs
 | otherwise = original
 where original = x:xs

-- contarSiCumple: cuenta cuantos elementos de una lista cumplen una condicion dadas
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple f ls = length [a | a <- ls, f a]

-- list2Vector2: convierte una lista de dos o más números en un Punto.
-- Lanza error si la lista no tiene al menos dos elementos.
list2Vector2 :: [Double] -> Punto
list2Vector2 [] = error ">> list2Vector2: La lista de entrada no puede estar vacía."
list2Vector2 [x] = error ">> list2Vector2: La lista de entrada debe tener al menos dos elementos."
list2Vector2 (x:y:_) = (x, y)

-- 5. PARSEO DEL NIVEL

-- Tipos sinónimos para celda y nivel (grid)
type Celda = Char
type Grid = [[Celda]]

-- parsearNivel: convierte una lista de lineas de texto en un Grid.
parsearNivel :: [String] -> Grid
parsearNivel xs = xs

-- esSolido: indica si una celda es una plataforma sólida.
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False

-- esMeta: indica si una celda es la meta del nivel.
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False

-- esVacio: indica si una celda está vacía.
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False

-- esEnemigo: indica si una celda marca el inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esVacio c || esMeta c)

-- posicionesMeta: dado el nivel completo, devuelve las posiciones (fila, columna) en las que aparece la meta
posicionesMeta :: Grid -> [(Int,Int)]
posicionesMeta nivel = 
 [(fila, columna) | 
        (fila, celdas) <- nivelPorFilas, (columna, celda) <- (zip [0..] celdas),
        esMeta celda]
 where nivelPorFilas = zip [0..] nivel

-- posicionesEnemigos (analoga extrayendo el identificador): dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo en formato (fila, columna, id)
posicionesEnemigos :: Grid -> [(Int,Int,String)]
posicionesEnemigos nivel =
 [(fila, col, [celda]) | (fila, celdas) <- nivelPorFilas,
                    (col, celda) <- (zip [0..] celdas),
                    esEnemigo celda]
 where nivelPorFilas = zip [0..] nivel

-- agruparRachas (span / recursión con acumulador de indice y manejo correcto de rachas al final de la fila o filas sin solidos): recorre una fila del nivel y agrupa las columnas '#' consecutivas en pares (inicio, longitud)
agruparRachas :: [Celda] -> [(Int, Int)]
agruparRachas celdas = buscarSolido celdas 0 0
 where 
    buscarSolido :: [Celda] -> Int -> Int -> [(Int, Int)]
    buscarSolido [] ini long 
    -- No habia racha previa
     | long == 0 = []
    -- Habia racha, la añadimos
     | otherwise = [((ini-long), long)]
    buscarSolido (c:cs) ini long
     -- Caso 1: no es solido y no estaba contando antes
     | (not. esSolido) c && (long == 0) = buscarSolido cs (ini+1) long
     -- Caso 2: es solido --> inicia racha o mantenla
     | esSolido c = buscarSolido cs (ini+1) (long+1)
     -- Caso 3: acaba la racha --> añadir
     | (not. esSolido) c && (long /=0 ) = ((ini - long), long):buscarSolido cs (ini+1) 0


-- BONUS: QUICKCHECK

-- La suma de vectores es conmutativa
prop_suma_conmutativa :: Punto -> Punto -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.

-- La suma de vectores es asociativa
prop_suma_asociativa :: Punto -> Punto -> Punto -> Bool
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
prop_distancia_no_negativa :: Punto -> Punto -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- *Main> quickCheck prop_distancia_no_negativa
-- +++ OK, passed 100 tests.

-- La distancia es simétrica
prop_distancia_simetrica :: Punto -> Punto -> Bool
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
-- En vez de usar una función a->Bool genérica, usaremos la funcion odd.
prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado ls = contarSiCumple odd ls <= length ls

-- *Main> quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.
