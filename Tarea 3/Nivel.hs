module Nivel
    ( ErrorNivel(..)
    , NivelData(..)
    , splitOn
    , buscarClave
    , parsearFichero
    , validarNivel
    , parsearNivelCompleto
    , tamCelda
    , posicionCelda
    , gridAPlataformas
    , metasDesdeGrid
    , enemigosDesdeGrid
    , parsearNiveles
    , todasLasPosiciones
    ) where

import Tipos
import Objeto
import Data.List
import Text.Read

data ErrorNivel
    = FormatoInvalido String
    | NivelSinMeta
    | NivelConVariasMetas Int
    deriving (Eq, Show)

data NivelData = NivelData
    { nombreNivel :: String
    , fondos      :: [Int]
    , gridNivel   :: Grid
    } deriving (Eq, Show)

-- Busca la primera línea que comienza por la clave indicada.
buscarClave :: String -> [String] -> Maybe String
buscarClave clave = foldr buscar Nothing
    where
        buscar linea resto =
            case stripPrefix clave linea of
                Just valor -> Just valor
                Nothing    -> resto

-- Divide una cadena utilizando el carácter indicado como separador.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b


-- Interpreta un fichero de nivel y devuelve sus datos o un error de formato.
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero xs = do
    let (cabecera, resto) = break (== "---") xs

    if null resto
        then Left (FormatoInvalido "falta el separador '---'")
        else Right ()

    let mapa = tail resto

    if null mapa
        then Left (FormatoInvalido "no hay mapa despues del separador")
        else Right ()

    nombre <- maybeToEither
        (FormatoInvalido "falta la clave 'NOMBRE: '")
        (buscarClave "NOMBRE: " cabecera)

    fondosTexto <- maybeToEither
        (FormatoInvalido "falta la clave 'FONDOS: '")
        (buscarClave "FONDOS: " cabecera)

    listaFondos <- maybeToEither
        (FormatoInvalido "los fondos contienen valores no numericos")
        (mapM readMaybe (splitOn ',' fondosTexto))

    return (NivelData nombre listaFondos (parsearGrid mapa))

-- F. Auxiliar: Convierte un Maybe en Either utilizando el error indicado.
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither e Nothing  = Left e


-- Comprueba que el nivel tenga exactamente una meta.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivel
    | n == 0 = Left NivelSinMeta
    | n > 1  = Left (NivelConVariasMetas n)
    | otherwise = Right nivel
    where
        n = length (filter esMeta (concat (gridNivel nivel)))


-- Interpreta un fichero y comprueba que el nivel sea válido.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto xs = parsearFichero xs >>= validarNivel


tamCelda :: Float
tamCelda = 40

-- Calcula la esquina inferior izquierda de una celda según su fila y columna.
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
    (fromIntegral j * tamCelda,
     fromIntegral (total - 1 - i) * tamCelda)

-- Convierte las celdas sólidas de un grid en plataformas.
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
    [ Plataforma (x, y, tamCelda, tamCelda)
    | (i, fila) <- zip [0..] grid
    , (j, Solido) <- zip [0..] fila
    , let (x, y) = posicionCelda total i j ]
    where
        total = length grid

-- Convierte las celdas META de un grid en objetos Meta.
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
    [ Meta (posicionCelda total i j)
    | (i, fila) <- zip [0..] grid
    , (j, META) <- zip [0..] fila
    ]
    where
        total = length grid

-- Convierte las celdas InicioEnemigo de un grid en enemigos centrados en sus celdas.
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
    [ Enemigo ident
        (x + tamCelda/2, y + tamCelda/2)
        (-40, 0)
        (tamCelda, tamCelda)
        Patrulla
    | (i, fila) <- zip [0..] grid
    , (j, InicioEnemigo ident) <- zip [0..] fila
    , let (x, y) = posicionCelda total i j
    ]
    where
        total = length grid

-- BONUS

-- Parsea y valida todos los niveles, devolviendo el primer error.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto

-- Devuelve todas las posiciones de la rejilla en orden por filas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones grid =
    (,) <$> [0 .. length grid - 1]
        <*> [0 .. length (head grid) - 1]