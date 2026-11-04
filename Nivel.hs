-- Lectura y validación de niveles (Either) y paso del mapa a los objetos del mundo
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

import qualified Data.List as L
import Objeto (Enemigo(..), Meta(..), Plataforma(..))
import Text.Read (readMaybe)
import Tipos (Celda(..), Grid, TipoEnemigo(..), Vector2D, esMeta, esSolido, parsearGrid)

-- Motivos por los que un nivel puede ser inválido
data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

-- Nivel ya interpretado: nombre, fondos y mapa
data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

-- Divide una cadena por un carácter separador
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b

-- Devuelve lo que sigue a la clave en la primera línea que empiece por ella
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (l:ls) = case L.stripPrefix clave l of
  Just resto -> Just resto
  Nothing    -> buscarClave clave ls

-- Convierte un Maybe en Either con un FormatoInvalido que lleva el mensaje dado
aEither :: String -> Maybe a -> Either ErrorNivel a
aEither msg = maybe (Left (FormatoInvalido msg)) Right

-- Separa cabecera y mapa por la línea '---' (ignorando líneas en blanco del mapa)
separarMapa :: [String] -> Either ErrorNivel ([String], [String])
separarMapa lineas = case break (== "---") lineas of
  (_, [])            -> Left (FormatoInvalido "falta el separador '---'")
  (cabecera, _:resto) ->
    let mapa = filter (not . null) resto
    in if null mapa
         then Left (FormatoInvalido "no hay mapa tras el separador '---'")
         else Right (cabecera, mapa)

-- Interpreta las líneas de un fichero de nivel (cabecera, '---' y mapa)
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
  (cabecera, mapa) <- separarMapa lineas
  nombre    <- aEither "falta la clave 'NOMBRE: '" (buscarClave "NOMBRE: " cabecera)
  fondosTxt <- aEither "falta la clave 'FONDOS: '" (buscarClave "FONDOS: " cabecera)
  fondosNum <- aEither "FONDOS contiene algo no numerico" (mapM readMaybe (splitOn ',' fondosTxt))
  return (NivelData nombre fondosNum (parsearGrid mapa))

-- Valida que el nivel tenga exactamente una meta
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd = case length (filter esMeta (concat (gridNivel nd))) of
  0 -> Left NivelSinMeta
  1 -> Right nd
  n -> Left (NivelConVariasMetas n)

-- Interpreta y después valida un nivel
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas = parsearFichero lineas >>= validarNivel

-- Lado de una celda del mapa
tamCelda :: Float
tamCelda = 40

-- Esquina inferior izquierda de la celda (fila i desde arriba, columna j) en un mapa de total filas
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
  (fromIntegral j * tamCelda, fromIntegral (total - 1 - i) * tamCelda)

-- Todas las celdas del mapa junto con su posición en el mundo
celdasConPosicion :: Grid -> [(Vector2D, Celda)]
celdasConPosicion g =
  [ (posicionCelda (length g) i j, c)
  | (i, fila) <- zip [0 ..] g
  , (j, c)    <- zip [0 ..] fila
  ]

-- Una plataforma por cada celda sólida
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas g =
  [ Plataforma (x, y, tamCelda, tamCelda)
  | ((x, y), c) <- celdasConPosicion g
  , esSolido c
  ]

-- Una meta por cada celda META
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid g =
  [ Meta p
  | (p, c) <- celdasConPosicion g
  , esMeta c
  ]

-- Un enemigo patrulla centrado en cada celda de inicio de enemigo
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid g =
  [ Enemigo ident (x + tamCelda / 2, y + tamCelda / 2) (-40, 0) (tamCelda, tamCelda) Patrulla
  | ((x, y), InicioEnemigo ident) <- celdasConPosicion g
  ]

-- Bonus: interpreta y valida varios niveles (traverse); devuelve todos o el primer error
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto

-- Bonus: todas las posiciones (fila, columna) de una rejilla rectangular, por filas (Applicative de listas)
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones g = (,) <$> [0 .. length g - 1] <*> [0 .. length (concat (take 1 g)) - 1]
