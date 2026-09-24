-- Tarea 1 - Tommaso Saccenti

-- import de quickcheck
import Test.QuickCheck

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
