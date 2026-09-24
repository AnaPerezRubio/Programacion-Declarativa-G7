-- Imports -> Types -> Funciones
import Test.QuickCheck

type Vector2 = (Float, Float)
-- los necesitamos en el punto 5
type Caja    = (Float, Float, Float, Float)
type Celda = Char
type Fila  = String
type Grid  = [Fila]

data Lado = Arriba | Abajo | Izquierda | Derecha 
    deriving (Show, Eq)

-- 1

-- suma 
sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- multiplicación
escalarVector :: Float -> Vector2 -> Vector2
escalarVector factor (x, y) = (factor * x, factor * y)

-- distancia entre 2 puntos
distancia :: Vector2 -> Vector2 -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


--  2

-- 1. El Tipo Sinónimo (0.5 puntos)    4 valores (x, y, ancho, alto)
type Caja = (Float, Float, Float, Float)

-- 2. La Función solapan (1.5 puntos)
solapan :: Caja -> Caja -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
    x1 < x2 + w2 && -- caja 1 izq invade caja 2 der
    x1 + w1 > x2 && -- caja 1 der invade caja 2 izq
    y1 < y2 + h2 && -- caja 1 arriba invade caja 2 abajo
    y1 + h1 > y2    -- caja 1 abajo invade caja 2 arriba

-- 3 Lado de colisión (1.5 puntos)
data Lado = Arriba | Abajo | Izquierda | Derecha     -- represento los 4 lados de una posible colisión
    deriving (Show, Eq)


-- dos cajas que colisionan, devuelve enque lado ocurre 
ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
    | minSolape == solapeArriba    = Arriba
    | minSolape == solapeAbajo     = Abajo
    | minSolape == solapeIzquierda = Izquierda
    | otherwise                    = Derecha
  where
    solapeArriba    = (y2 + h2) - y1     -- Solape = TechoCaja2 - SueloCaja1 = (y2 + h2) - y1
    solapeAbajo     = (y1 + h1) - y2     -- Solape = TechoCaja1 - SueloCaja2 = (y1 + h1) - y2
    solapeIzquierda = (x1 + w1) - x2     -- Solape = DerechaCaja1 - IzquierdaCaja2 = (x1 + w1) - x2
    solapeDerecha   = (x2 + w2) - x1     -- Solape = DerechaCaja2 - IzquierdaCaja1 = (x2 + w2) - x1
    
    -- minimum busca el número más pequeño de una lista
    minSolape = minimum [solapeArriba, solapeAbajo, solapeIzquierda, solapeDerecha]    -- minium se usa para seleccionar el menor, pues donde menor se mete un elemento sobre el otro es donde se produce la colisión



-- 4 Utilidades de listas y cadenas 

-- 1. splitOn   0.6
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]                            -- caso base: si la palabra está vacía, nos queda una lista con un trozo vacío
splitOn separador (x:xs)                       -- caso recursivo: nos fijamos en la primera letra(x) y el resto(xs)
    | x == separador = "" : resto
    | otherwise      = (x : head resto) : tail resto
  where 
    resto = splitOn separador xs

-- 2. trim    0.4
quitarEspacios :: String -> String    -- función que limpia solo por la izq
quitarEspacios "" = ""
quitarEspacios (c:cs)
    | c == ' ' || c == '\t' || c == '\n' = quitarEspacios cs
    | otherwise                          = c : cs

trim :: String -> String
trim cadena = reverse (quitarEspacios (reverse (quitarEspacios cadena)))  -- función para limpiar por la derecha

-- 3. contarSiCumple   0.5      pasar un filtro/condición
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple condicion lista = length [x | x <- lista, condicion x]

-- 4. list2Vector2   0.5   transformo una lista en el tipo ya estructurado Vector2
list2Vector2 :: [Float] -> Vector2
list2Vector2 []      = error "Lista vacia no posible convertir en Vector2"                       -- lista vacía
list2Vector2 [_]     = error "Falta un elemento en la lista para convertir en Vector2"           -- lista con un solo elemento 
list2Vector2 (x:y:_) = (x, y)



-- 5 Parseo del nivel

-- 1. Funciones básicas de parseo y comprobación               1.1 
parsearNivel :: [String] -> Grid                     -- lista de textos y la convierte en nuestro tipo Grid. Dado que Grid y [String] son lo mismo, la función solo devuelve lo que le entra.
parsearNivel nivel = nivel

esSolido :: Celda -> Bool     -- celda suelo/pared
esSolido '#' = True
esSolido _   = False

esMeta :: Celda -> Bool     -- celda meta
esMeta 'M' = True
esMeta _   = False

esVacio :: Celda -> Bool   -- celda aire
esVacio '.' = True
esVacio _   = False


esEnemigo :: Celda -> Bool         -- enemigo es cualquier letra salvo si es sólido, meta, vacío
esEnemigo c = not (esSolido c) && not (esMeta c) && not (esVacio c)




-- 2. Buscar coordenadas con zip y Listas por Comprensión       0.8
posicionesMeta :: Grid -> [(Int, Int)]               -- devuelve lista de posiciones (fila, columna) donde se encuentra la meta
posicionesMeta nivel = 
    [ (f, c) 
    | (f, fila) <- zip [0..] nivel    -- enumera las filas del nivel
    , (c, celda) <- zip [0..] fila    -- enumera cada celda dentro de la fila
    , esMeta celda                    -- filtra celdas meta
    ]

posicionesEnemigos :: Grid -> [(Int, Int, String)]      -- devuelve lista de posiciones e identificador del enemigo
posicionesEnemigos nivel = 
    [ (f, c, [celda]) 
    | (f, fila) <- zip [0..] nivel
    , (c, celda) <- zip [0..] fila
    , esEnemigo celda 
    ]

-- 3. Agrupar las plataformas sólidas       1.1
agruparRachas :: Fila -> [(Int, Int)]                    -- agrupa plataformas sólidas continuas de una fila y devuelvo su índice inicial y longitud
agruparRachas fila = medirRacha 0 fila 
  where
    medirRacha _ "" = []      
    medirRacha indice cadena@(c:cs)
        | esSolido c = (indice, length racha) : medirRacha (indice + length racha) resto 
        | otherwise  = medirRacha (indice + 1) cs
      where 
        (racha, resto) = span esSolido cadena



-- 6. Bonus: Propiedades con QuickCheck
prop_suma_conmutativa :: Vector2 -> Vector2 -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a      -- suma de vectores es conmutativa: a + b = b + a

prop_suma_asociativa :: Vector2 -> Vector2 -> Vector2 -> Bool
prop_suma_asociativa a b c =  sumaVectores (sumaVectores a b) c == sumaVectores a (sumaVectores b c)         -- suma de vectores es asociativa: (a + b) + c = a + (b + c)

prop_escalar_neutro :: Vector2 -> Bool
prop_escalar_neutro v = escalarVector 1.0 v == v       -- escalar un vector por 1.0 devuelve el mismo vector exacto

prop_distancia_no_negativa :: Vector2 -> Vector2 -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0.0         -- distancia euclídea entre dos puntos nunca es negativa

prop_distancia_simetrica :: Vector2 -> Vector2 -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a    -- distancia desde a hasta b == distancia desde b hasta a

prop_solapan_simetrica :: Caja -> Caja -> Bool                
prop_solapan_simetrica c1 c2 = solapan c1 c2 == solapan c2 c1    -- si A solapa con B, B solapa con A o

prop_trim_idempotente :: String -> Bool
prop_trim_idempotente cadena = trim (trim cadena) == trim cadena    -- cuando limpio espacios 2 veces seguidas == 1 vez

prop_splitOn_sin_separador :: Char -> String -> Property         -- al hacer splitOn con un separador inexistente devuelve lo original
prop_splitOn_sin_separador c cadena = 
    not (c `elem` cadena) ==> splitOn c cadena == [cadena]

prop_contarSiCumple_acotado :: Int -> [Int] -> Bool            -- comprobación de que el recuento de elementos filtrados es <= a la longitud total
prop_contarSiCumple_acotado pivote lista = 
    contarSiCumple (> pivote) lista <= length lista

