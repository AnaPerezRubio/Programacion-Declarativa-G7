-- modulo para leer y comprobar niveles
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

-- import cualificado de Data.List
import qualified Data.List as L
import Text.Read (readMaybe)

-- imports de codigo de otro fichero
import Objeto (Enemigo(..), Meta(..), Plataforma(..))
import Tipos (Celda(..), Grid, TipoEnemigo(..), Vector2D, esMeta, parsearGrid)

-- errores posibles de un nivel
data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

-- datos de un nivel
data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

-- funcion que parte una cadena por un caracter.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _:b) -> a : splitOn c b

-- funcion wue busca el valor de una clave
buscarClave :: String -> [String] -> Maybe String
buscarClave clave ls = case [r | Just r <- map (L.stripPrefix clave) ls] of
  (r:_) -> Just r
  []    -> Nothing

-- como buscarClave pero con error si falta la clave
buscarOError :: String -> [String] -> Either ErrorNivel String
buscarOError clave ls = case buscarClave clave ls of
  Nothing -> Left (FormatoInvalido ("falta la clave '" ++ clave ++ "'"))
  Just v  -> Right v

-- funcion que lee un nivel desde sus lineas
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero ls = do
  let cabecera = takeWhile (/= "---") ls
      resto    = dropWhile (/= "---") ls
  lineasMapa <- case resto of
    []      -> Left (FormatoInvalido "falta el separador '---'")
    (_:m)   -> Right (filter (not . null) m)
  if null lineasMapa
    then Left (FormatoInvalido "no hay mapa tras el separador '---'")
    else Right ()
  nombre   <- buscarOError "NOMBRE: " cabecera
  txtFonds <- buscarOError "FONDOS: " cabecera
  fs <- case mapM readMaybe (splitOn ',' txtFonds) of
    Nothing -> Left (FormatoInvalido "FONDOS contiene valores no numericos")
    Just xs -> Right xs
  return (NivelData nombre fs (parsearGrid lineasMapa))

-- funcion que comprueba que hay una sola meta.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd = case length (filter esMeta (concat (gridNivel nd))) of
  0 -> Left NivelSinMeta
  1 -> Right nd
  n -> Left (NivelConVariasMetas n)

-- funcion que lee y comprueba un nivel.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto ls = parsearFichero ls >>= validarNivel

-- el tamaño de una celda
tamCelda :: Float
tamCelda = 40

-- funcion que devuelve la posicion de una celda en el mundo
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
  (fromIntegral j * tamCelda, fromIntegral (total - 1 - i) * tamCelda)

-- funcion que devuelva las plataformas de mapa
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas g =
  [ Plataforma (cajaCelda (posicionCelda total i j))
  | (i, fila) <- zip [0 ..] g
  , (j, Solido) <- zip [0 ..] fila
  ]
  where
    total = length g
    cajaCelda :: Vector2D -> (Float, Float, Float, Float)
    cajaCelda (x, y) = (x, y, tamCelda, tamCelda)

-- funcion que devuelve la meta de mapa
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid g =
  [ Meta (posicionCelda total i j)
  | (i, fila) <- zip [0 ..] g
  , (j, META) <- zip [0 ..] fila
  ]
  where total = length g

-- funcion que devuelve enemigos del mapa
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid g =
  [ Enemigo ident (centro (posicionCelda total i j)) (-40, 0) (tamCelda, tamCelda) Patrulla
  | (i, fila) <- zip [0 ..] g
  , (j, InicioEnemigo ident) <- zip [0 ..] fila
  ]
  where
    total = length g
    centro :: Vector2D -> Vector2D
    centro (x, y) = (x + tamCelda / 2, y + tamCelda / 2)

-- lee varios niveles
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles = traverse parsearNivelCompleto

-- todas las posiciones de mapa
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones g = (,) <$> [0 .. length g - 1] <*> [0 .. ancho g - 1]
  where
    ancho :: Grid -> Int
    ancho []      = 0
    ancho (f : _) = length f
