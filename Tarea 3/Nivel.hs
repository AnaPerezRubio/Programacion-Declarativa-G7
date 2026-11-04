-- =============================================================
-- NIVEL.hs
-- =============================================================

module Nivel (
    ErrorNivel(..), NivelData(..), splitOn, buscarClave, 
    parsearFichero, validarNivel, parsearNivelCompleto,
    tamCelda, posicionCelda, gridAPlataformas, metasDesdeGrid,
    enemigosDesdeGrid, parsearNiveles, todasLasPosiciones
) where

import Tipos (Vector2D, Celda(..), Grid, TipoEnemigo(..),
              parsearGrid, esMeta)
import Objeto (Enemigo(..), Plataforma(..), Meta(..))
import Text.Read (readMaybe)
import Control.Monad (when)

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

-- Divide una cadena según un separador
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b

-- Devuelve lo que sigue a la clave en la primera línea que empiece por ella
buscarClave :: String -> [String] -> Maybe String
buscarClave _ []           = Nothing
buscarClave prefijo (l:ls) =
     case quitaPrefijo prefijo l of
       Just resto -> Just resto
       Nothing -> buscarClave prefijo ls
  where
   quitaPrefijo :: String -> String -> Maybe String
   quitaPrefijo [] restante = Just restante
   quitaPrefijo (p:ps) (x:xs)
     | p == x = quitaPrefijo ps xs
     | otherwise = Nothing
   quitaPrefijo _ _ = Nothing

-- Convierte una lista de cadenas en un NivelData o emite el error correspondiente
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
  let (cabecera, resto) = span (/= "---") lineas 
  when (null resto) $ Left (FormatoInvalido "falta el separador '---'")
  
  let mapa = tail resto -- Elimino el separador
  when (null mapa) $ Left (FormatoInvalido "No existe mapa despues de '---'.")
  
  nombre <- case buscarClave "NOMBRE: " cabecera of
              Nothing -> Left (FormatoInvalido "falta la clave 'NOMBRE: '")
              Just n -> Right n
 
  fondosStr <- case buscarClave "FONDOS: " cabecera of
                 Nothing -> Left (FormatoInvalido "falta la clave 'FONDOS: '")
                 Just f -> Right f

  fondos <- case mapM readMaybe (splitOn ',' fondosStr) of
              Nothing -> Left (FormatoInvalido "Los fondos no son numéricos.")
              Just ls -> Right ls

  let grid = parsearGrid mapa
  
  return (NivelData nombre fondos grid)

-- Comprueba si un nivel tiene el formato correcto y emite un error 
-- descriptivo en caso negativo
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivelDatos = case numMetas of
    0 -> Left NivelSinMeta
    1 -> Right nivelDatos
    m -> Left (NivelConVariasMetas m)
  where numMetas = length [ c | fila <- gridNivel nivelDatos, c <- fila, esMeta c]

-- Convierte una lista de cadenas en NivelData, validando que el formato es correcto
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto xs = parsearFichero xs >>= validarNivel

-- Tamaño de celda predefinido
tamCelda :: Float 
tamCelda = 40

-- Devuelve un vector con la posicion de una celda dada por su 
-- esquina inferior izquierda (i,j) y el total de las filas
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j = (x, y )
 where x = fromIntegral j*tamCelda
       y = fromIntegral (total-1-i)*tamCelda

-- Conveierte un Grid a una lista de plataformas 
-- (solo tenemos el cuenta las celdas solidas)
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
 [ Plataforma (x, y, tamCelda, tamCelda) 
 | let total = length grid
 , (i, fila) <- zip [0..] grid
 , (j, Solido) <- zip [0..] fila
 , let (x,y) = posicionCelda total i j ]

-- Da una lista de los objetos meta a partir de un grid
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid = 
 [ Meta (x,y)
 | let total = length grid
 , (i, fila) <- zip [0..] grid
 , (j, META) <- zip [0..] fila
 , let (x,y)=posicionCelda total i j]

-- Da una lista de los enemigos a partir de un grid
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid = 
 [ Enemigo {ident = id,
            posEnemigo = (x + tamCelda/2, y + tamCelda/2),
            velEnemigo = (-40,0),
            dimE = (tamCelda, tamCelda),
            tipo = Patrulla} 
 | let total = length grid 
 , (i, fila) <- zip [0..] grid
 , (j, InicioEnemigo id) <- zip [0..] fila
 , let (x,y) = posicionCelda total i j ]

-- -------------------------------------------------------------
-- BONUS: 
-- -------------------------------------------------------------

-- parsearNiveles: con mapM.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles xss = mapM parsearNivelCompleto xss

-- todasLasPosiciones: <$>, <*> sobre listas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones grid@(fila:_) = (,) <$> [0 .. length grid - 1] <*> [0 .. length fila - 1]