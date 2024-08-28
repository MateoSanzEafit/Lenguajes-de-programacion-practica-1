import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

data Producto = Producto {
    nombre :: String,
    categoria :: String,
    cantidad :: Int
} deriving (Show, Read)

registrarProducto :: String -> String -> Int -> [Producto] -> [Producto]
registrarProducto nombreProducto categoriaProducto cantidadProducto inventario =
    let inventarioActualizado = map (\p -> if nombreProducto == nombre p
                                           then p { cantidad = cantidad p + cantidadProducto }
                                           else p) inventario
    in if any (\p -> nombreProducto == nombre p) inventario
       then inventarioActualizado
       else Producto nombreProducto categoriaProducto cantidadProducto : inventario

buscarPorCategoria :: String -> [Producto] -> [Producto]
buscarPorCategoria categoriaProducto inventario =
    filter (\p -> categoriaProducto == categoria p) inventario

listarProductos :: [Producto] -> IO ()
listarProductos [] = putStrLn "no hay productos en el inventario."
listarProductos inventario = do
    putStrLn "productos en el inventario:"
    mapM_ (putStrLn . mostrarProducto) inventario

mostrarProducto :: Producto -> String
mostrarProducto (Producto nombre categoria cantidad) =
    "producto \n nombre: \"" ++ nombre ++ "\"\n categoria: \"" ++ categoria ++ "\"\n cantidad: " ++ show cantidad

contarProductosPorCategoria :: String -> [Producto] -> Int
contarProductosPorCategoria categoriaProducto inventario =
    sum $ map cantidad $ buscarPorCategoria categoriaProducto inventario

guardarInventario :: [Producto] -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show inventario))
    putStrLn "inventario guardado en el archivo inventario.txt."

cargarInventario :: IO [Producto]
cargarInventario = do
    contenido <- withFile "inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map read lineas)

main :: IO ()
main = do
    inventario <- cargarInventario
    putStrLn "bienvenido al sistema de gestion de inventario!"
    cicloPrincipal inventario

cicloPrincipal :: [Producto] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nseleccione una opcion:"
    putStrLn "1. registrar entrada de producto"
    putStrLn "2. buscar producto por categoria"
    putStrLn "3. listar productos"
    putStrLn "4. contar productos por categoria"
    putStrLn "5. salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "ingrese el nombre del producto:"
            nombreProducto <- getLine
            putStrLn "ingrese la categoria del producto:"
            categoriaProducto <- getLine
            putStrLn "ingrese la cantidad del producto:"
            cantidadProducto <- readLn
            let inventarioActualizado = registrarProducto nombreProducto categoriaProducto cantidadProducto inventario
            guardarInventario inventarioActualizado
            cicloPrincipal inventarioActualizado

        "2" -> do
            putStrLn "ingrese la categoria del producto a buscar:"
            categoriaProducto <- getLine
            let productos = buscarPorCategoria categoriaProducto inventario
            if null productos
                then putStrLn "no se encontrron productos en esta categoria."
                else mapM_ (putStrLn . mostrarProducto) productos
            cicloPrincipal inventario

        "3" -> do
            listarProductos inventario
            cicloPrincipal inventario

        "4" -> do
            putStrLn "ingrese la categoria del producto para contar:"
            categoriaProducto <- getLine
            let cantidadTotal = contarProductosPorCategoria categoriaProducto inventario
            putStrLn $ "cantidad total de productos en la categoria \"" ++ categoriaProducto ++ "\": " ++ show cantidadTotal
            cicloPrincipal inventario

        "5" -> putStrLn "¡hasta luego!"

        _ -> do
            putStrLn "opcion no valida. por favor, seleccione una opcion validad."
            cicloPrincipal inventario
