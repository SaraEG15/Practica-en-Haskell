import Data.List
import System.IO
import Control.DeepSeq (deepseq)
import System.Directory (doesFileExist)

data Articulo = Articulo { 
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

-- Función para registrar la entrada del articulo
registrarEntrada :: String -> String -> [Articulo] -> [Articulo]
registrarEntrada nombreArticulo categoriaArticulo articulos = 
    Articulo nombreArticulo categoriaArticulo : articulos

-- Función para buscar un articulo por su categoria
buscarArticulo :: String -> [Articulo] -> Maybe Articulo
buscarArticulo categoriaArticulo articulos =
    find (\v -> categoriaArticulo == categoria v) articulos

-- Función para guardar la información de los articulos
guardarArticulo :: [Articulo] -> IO ()
guardarArticulo articulos = do
    withFile "articulo.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarArticulo articulos))
    putStrLn "Articulo guardado en articulo.txt."

-- Función para cargar la información de los articulos desde un archivo de texto
cargarArticulo :: IO [Articulo]
cargarArticulo = do
    existe <- doesFileExist "articulo.txt"
    if existe
        then do
            contenido <- withFile "articulo.txt" ReadMode $ \h -> do
                contenido <- hGetContents h
                contenido `deepseq` return contenido
            let lineas = lines contenido
            return (map leerArticulo lineas)
        else return []  -- Si el archivo no existe, retorna una lista vacía
    where
      leerArticulo linea = read linea :: Articulo

-- Función para mostrar la información de los articulos como cadena de texto
mostrarArticulo :: Articulo -> String
mostrarArticulo (Articulo nombreArticulo categoriaArticulo) =
     "Articulo {Nombre: \"" ++ nombreArticulo ++ "\", Categoria: \"" ++ categoriaArticulo ++ "\"}"

-- Función para listar los articulos
listarArticulo :: [Articulo] -> IO ()
listarArticulo [] = putStrLn "No hay articulos disponibles."
listarArticulo articulos = do
    putStrLn "Articulos disponibles:"
    mapM_ (putStrLn . mostrarArticulo) articulos 

-- Función para contar los artículos de una categoría específica
contarArticulosPorCategoria :: String -> [Articulo] -> Int
contarArticulosPorCategoria categoriaArticulo articulos =
    length $ filter (\a -> categoria a == categoriaArticulo) articulos

-- Función principal del programa
main :: IO ()
main = do
    articulos <- cargarArticulo
    cicloPrincipal articulos

-- Ciclo principal del programa
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal articulos = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese el nombre de la categoría del artículo:"
            nombreCategoria <- getLine
            let articuloInventario = registrarEntrada nombreArticulo nombreCategoria articulos
            guardarArticulo(articuloInventario)
            cicloPrincipal articuloInventario

        "2" -> do
            putStrLn "Ingrese el nombre de la categoría del artículo a buscar:"
            nombreCategoria <- getLine
            case buscarArticulo nombreCategoria articulos of
                Just articuloEncontrado -> do
                    putStrLn "Artículo encontrado:"
                    putStrLn $ mostrarArticulo articuloEncontrado
                Nothing -> putStrLn "No se encontró ningún artículo con esa categoría."
            cicloPrincipal articulos    

        "3" -> do
            listarArticulo articulos
            cicloPrincipal articulos

        "4" -> do
            putStrLn "Ingrese el nombre de la categoría del artículo:"
            nombreCategoria <- getLine
            let cantidadArticulos = contarArticulosPorCategoria nombreCategoria articulos
            putStrLn $ "Cantidad de artículos en la categoría " ++ nombreCategoria ++ " es " ++ show cantidadArticulos
            cicloPrincipal articulos

        "5" -> putStrLn "Saliendo del programa, Adios!"

        _ -> do
            putStrLn "Opción inválida. Por favor, seleccione una opción válida."
            cicloPrincipal articulos
