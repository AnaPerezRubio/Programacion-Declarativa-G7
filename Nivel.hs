import Test.QuickCheck
--EJERCICIO 1 VECTORES2D 

type Punto = (Double, Double)

-- Función: sumaVectores
-- Suma componente a componente dos vectores/puntos 2D
sumaVectores :: Punto -> Punto -> Punto
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

---------------------------------------------------------------------

-- Función: escalarVector
-- Multiplica un vector/punto 2D por un factor escalar
escalarVector :: Double -> Punto -> Punto
escalarVector k (x, y) = (k * x, k * y)

---------------------------------------------------------------------

-- Función: distancia
-- Calcula la distancia euclídea entre dos puntos 2D
distancia :: Punto -> Punto -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

---------------------------------------------------------------------
-- EJERCICIO 2 CAJAS DE COLISION

type Caja = (Double, Double, Double, Double)

-- Función: solapan
-- Indica si dos cajas delimitadoras se solapan (colisionan)
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
    x1 < x2 + w2 && x2 < x1 + w1 &&
    y1 < y2 + h2 && y2 < y1 + h1

---------------------------------------------------------------------
--EJERCICIO 3 SOLAPAMIENTO DE CAJAS

type Lado = String

-- Función: ladoColision
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
    | oArriba <= oAbajo && oArriba <= oIzquierda && oArriba <= oDerecha = "Arriba"
    | oAbajo <= oIzquierda && oAbajo <= oDerecha                       = "Abajo"
    | oIzquierda <= oDerecha                                            = "Izquierda"
    | otherwise                                                         = "Derecha"
  where
    oArriba    = (y2 + h2) - y1
    oAbajo     = (y1 + h1) - y2
    oIzquierda = (x1 + w1) - x2
    oDerecha   = (x2 + w2) - x1

---------------------------------------------------------------------
--EJERCICIO 4 UTILIDADES DE LISTAS Y PARSEOS

-- Función: splitOn
-- Divide una cadena en trozos cada vez que aparece un carácter separador dado
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

---------------------------------------------------------------------
-- Función: esEspacio
-- Indica si un carácter es un espacio en blanco (espacio, tabulador o salto de línea)
esEspacio :: Char -> Bool
esEspacio c = c == ' ' || c == '\t' || c == '\n'

-- Función: trim
-- Elimina los espacios en blanco (espacios, tabuladores, saltos de línea)
-- al principio y al final de una cadena
trim :: String -> String
trim = trimFinal . trimInicio
  where
    trimInicio [] = []
    trimInicio (x:xs)
        | esEspacio x = trimInicio xs
        | otherwise   = x : xs

    trimFinal [] = []
    trimFinal (x:xs)
        | esEspacio x && null resto = []
        | otherwise                 = x : resto
      where
        resto = trimFinal xs

---------------------------------------------------------------------

-- Función: contarSiCumple
-- Cuenta cuántos elementos de una lista cumplen una condición dada
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple f xs = length [x | x <- xs, f x]

---------------------------------------------------------------------

-- Función: list2Vector2
-- Convierte una lista de dos (o más) números en un vector/punto 2D;
-- lanza un error si la lista no tiene al menos dos elementos
list2Vector2 :: [Double] -> Punto
list2Vector2 [] = error "Lista vacia no posible convertir en Punto"
list2Vector2 [x] = error "Falta un elemento en la lista para convertir en Punto"
list2Vector2 (x:y:_) = (x, y)


--EJERCICIO 5 PARSEO DEL NIVEL

type Celda = Char
type Fila = [Celda]
type Grid = [Fila]
type Posicion = (Int, Int)


-- Función: parsearNivel
-- Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos que representa el nivel completo
parsearNivel :: [String] -> Grid
parsearNivel = id

---------------------------------------------------------------------

-- Función: esSolido
-- Indica si una celda es una plataforma sólida
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

---------------------------------------------------------------------

-- Función: esMeta
-- Indica si una celda es la meta del nivel
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

---------------------------------------------------------------------

-- Función: esVacio
-- Indica si una celda está vacía (no hay nada en ella)
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

---------------------------------------------------------------------

-- Función: esEnemigo
-- Indica si una celda marca el punto de inicio de un enemigo
-- (cualquier carácter que no sea sólido, meta ni vacío)
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c) && not (esMeta c) && not (esVacio c)

---------------------------------------------------------------------
 
-- Función: posicionesMeta
-- Dado el nivel completo, devuelve la lista de posiciones (fila, columna)
-- en las que aparece la meta
posicionesMeta :: Grid -> [Posicion]
posicionesMeta grid =
    [(f, c) | (f, fila) <- zip [0..] grid, (c, celda) <- zip [0..] fila, esMeta celda]

---------------------------------------------------------------------

-- Función: posicionesEnemigos
-- Dado el nivel completo, devuelve la lista de posiciones y el identificador
-- de cada enemigo (fila, columna, identificador)
posicionesEnemigos :: Grid -> [(Int, Int, String)]
posicionesEnemigos grid =
    [(f, c, [celda]) | (f, fila) <- zip [0..] grid, (c, celda) <- zip [0..] fila, esEnemigo celda]

---------------------------------------------------------------------

-- Función: agruparRachas
-- Recorre una fila del nivel y agrupa las columnas '#' consecutivas
-- en pares (columna de inicio, longitud de la racha)
agruparRachas :: Fila -> [(Int, Int)]
agruparRachas fila = ir 0 fila
  where
    ir :: Int -> Fila -> [(Int, Int)]
    ir _ [] = []
    ir idx xs =
        let (noSolidos, resto1) = span (not . esSolido) xs
            idxTrasHuecos = idx + length noSolidos
        in case resto1 of
             []  -> []
             _   -> let (solidos, resto2) = span esSolido resto1
                        longitud = length solidos
                    in (idxTrasHuecos, longitud) : ir (idxTrasHuecos + longitud) resto2

---------------------------------------------------------------------
