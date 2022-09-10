-- -----------------------------------------------------------------------------
-- Programación Declarativa 2022/23
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Septiembre (segunda convocatoria)                     7 de Septiembre de 2022
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Array
import System.IO
import Data.List
import Text.CSV

-- -----------------------------------------------------------------------------
-- Ejercicio 1 (2,5 puntos)
-- Sheldon, Leonard, Penny, Rajesh y Howard están haciendo cola para una máquina
-- de vending llamada "doble cola". No hay más gente esperando en la cola. La
-- primera persona (Sheldon) compra una lata, bebe de ella y se duplica! Esto 
-- resulta en dos Sheldon que se van al final de la cola. Después viene el
-- siguiente (Leonard), compra una lata, bebe, se duplica y los dos Leonards
-- se van al final de la cola, y así sucesivamente. Se pide escribir una función
-- que nos diga el nombre de la persona que bebe la n-ésima coca cola.
-- Pista: Hay varias formas de solucionar este ejercicio, por ejemplo con listas
-- infinitas definidas con recursión sin caso base, o funciones como replicate, 
-- iterate o cycle pueden ser útiles.

-- Por ejemplo,
-- > nCola personas 1
-- "Sheldon"
-- > nCola personas 52
-- "Penny"
-- > nCola personas 10010
-- "Howard"
-- -----------------------------------------------------------------------------

personas :: [String]
personas = ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"]

-- Solución recursiva
nColaAux :: [String] -> [Int] -> [String]
nColaAux (p:ps) (n:ns) = (replicate n p) ++ (nColaAux (ps++[p]) (ns++[2*n]))

nCola :: [String] -> Int -> String
nCola ps n = cola !! (n-1) 
    where cola = nColaAux ps (replicate (length ps) 1)

-- Solución con cycle e iterate
nColaAux2 :: [String] -> [String]
nColaAux2 ps = concat [ replicate i p | (p,i) <- zip (cycle ps) ns' ]
    where ns' = concat $ iterate (map (*2)) ns 
          ns = (replicate (length ps) 1)

nCola2 :: [String] -> Int -> String
nCola2 ps n = nColaAux2 ps !! (n-1) 

-- Solución con solo recursividad
nCola3 :: [String] -> Int -> String
nCola3 ps n = 
    let p = head ps
        l = length ps
    in if n <= l
        then ps !!(n-1)
        else nCola3 ((tail ps)++[p,p]) (n-1)

-- Solución con solo recursividad 2
nCola4 :: [String] -> Int -> String
nCola4 ps 1 = head ps
nCola4 ps n = nCola4 ((tail ps)++[p,p]) (n-1)
    where p = head ps

-- Comparación de tiempos
-- *Main> nCola personas 10010 
-- "Howard"
-- (0.01 secs, 1,244,592 bytes)
-- *Main> nCola2 personas 10010
-- "Howard"
-- (0.01 secs, 1,220,496 bytes)
-- *Main> nCola3 personas 10010
-- "Howard"
-- (1.01 secs, 704,258,648 bytes)
-- *Main> nCola4 personas 10010
-- "Howard"
-- (1.92 secs, 2,227,369,880 bytes)

-- -----------------------------------------------------------------------------
-- Ejercicio 2 (2,5 puntos)

-- Definiremos un árbol genérico con el siguiente tipo de dato algebraico:

-- data Arbol a = N a [Arbol a] deriving Show

-- a) Necesitamos marcar valores del árbol como eliminados. A continuación,
-- extiende la definición anterior para que un árbol también pueda ser
-- un nodo sin valor de tipo a asociado, pero con una lista de árboles.
-- Llama al constructor "R".

data Arbol a = N a [Arbol a] | R [Arbol a]
    deriving Show

-- b) Define la función (eliminaRama rs a), tal que dado un árbol genérico a y
-- una lista rs que representa una posible rama del árbol, devuelva el árbol
-- eliminando todas las repeticiones de dicha rama. La eliminación de un 
-- valor en el árbol se hace simplemente cambiando el constructor del nodo N
-- por R. Entendemos por rama una sucesión de elementos que están en el árbol,
-- siguiendo orden de jerarquía: es decir, el primer elemento es padre del segundo,
-- el segundo es padre del tercero, etc. Por ejemplo, supongamos el siguiente árbol:
--
--        __5___ 
--       |  |   |
--       3  2  _5___
--       |    | | | |
--       0    1 0 2 3
-- 
-- Si eliminamos la rama [5,3,0], obtendríamos:
--
--        __R___ 
--       |  |   |
--       R  2  _5___
--       |    | | | |
--       R    1 0 2 3
-- 
-- Sin embargo, al intentar eliminar la rama [6,7] obtenemos el árbol sin 
-- modificar, ya que tal rama no existe. Por último, al eliminar la rama [5,3],
-- tendríamos que eliminar todas las apariciones:
--
--        __R___ 
--       |  |   |
--       R  2  _R___
--       |    | | | |
--       0    1 0 2 R
--
-- Nota 1: Por simplicidad, puedes asumir que los nodos R del árbol de entrada
-- no forman parte de ninguna rama.
-- Nota 2: Si la rama es vacía, no se elimina ningún nodo en el árbol.
--
-- Por ejemplo:
-- > eliminaRama [5,3,0] arbol1
-- R [R [R []],N 2 [],N 5 [N 1 [],N 0 [],N 2 [],N 3 []]]
-- > eliminaRama [5,3] arbol1  
-- R [R [N 0 []],N 2 [],N 5 [N 1 [],N 0 [],N 2 [],R []]]
-- > eliminaRama [3,5] arbol1
-- N 5 [N 3 [N 0 []],N 2 [],N 5 [N 1 [],N 0 [],N 2 [],N 3 []]]
-- > eliminaRama [1,1] arbol2
-- R [N 0 [R []],R [R [],N 0 []]]
-- > eliminaRama [0,1] arbol2
-- N 1 [R [R []],N 1 [N 1 [],N 0 []]]
-- > eliminaRama [] arbol2   
-- N 1 [N 0 [N 1 []],N 1 [N 1 [],N 0 []]]

arbol1, arbol2 :: Arbol Int
arbol1 = N 5 [N 3 [N 0 []], N 2 [], N 5 [N 1 [], N 0 [], N 2 [], N 3 []]]
arbol2 = N 1 [N 0 [N 1 []], N 1 [N 1 [], N 0 []]]

eliminaRama :: Eq a => [a] -> Arbol a -> Arbol a 
eliminaRama [] a = a 
eliminaRama rs a@(N v as) 
     | esRamaInmediata rs a = R (map (eliminaRama rs . eliminaRama (tail rs)) as)
     | otherwise = N v (map (eliminaRama rs) as)
eliminaRama _ (R as) = R as      

esRamaInmediata :: Eq a => [a] -> Arbol a -> Bool
esRamaInmediata [] _ = True
esRamaInmediata [r] (N v []) = r == v
esRamaInmediata (r:rs) (N v as) = (v == r && any (esRamaInmediata rs) as) 
esRamaInmediata _ _ = False


-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Ejercicio 3 (2,5 puntos)
-- En redes neuronales, la función de pooling es muy importante. En este
-- ejercicio se pide realizar una implementación muy básica. En resumen, esta 
-- función recibe una matriz m, un tamaño de ventana v y una función f, y 
-- devuelve una matriz que es el resultado de aplicar f a las submatrices de
-- tamaño v x v (sin solapamiento) de la matriz m. Veámoslo con más detalle.
-- Específicamente, diremos que la función de pooling recibe:
--   * una matriz m cuadrada de tamaño n x n, con n >= 1. Si la matriz no es
--     cuadrada, entonces devuelve Nothing.
--   * un tamaño de ventana v. Como restricción, pediremos que v sea divisor 
--     de n, y v >= 1. Si no es así, la función devuelve Nothing. 
--   * una función de agregación f sobre listas, por ejemplo, suma, máximo, mínimo
--     media, etc.
-- La función de pooling funciona como sigue (ver el ejemplo de abajo):
--   * la matriz resultado tiene un tamaño de n' x n', con n' = n / v
--   * se forman ventanas (submatrices) de v x v de la matriz m, sin solapamiento.
--   * la matriz resultado tiene una posición por ventana, que corresponde al 
--     resultado de aplicar la función f a los elementos de la ventana 
--     correspondiente.
--
-- Para aclararlo, pongamos un ejemplo. Supongamos que n = 4, v = 2, y m es
--                                                
--  ┌         ┐                                   
--  │ 1 2 3 4 |                                  
--  │ 5 6 7 8 |   
--  │ 9 8 7 6 |                                   
--  │ 5 4 3 2 |                                  
--  └         ┘                                  
--
-- la divisón de la matriz con una ventana de 2x2 es:  

--  ┌     |     ┐ 
--  │ 1 2 | 3 4 |  
--  │ 5 6 | 7 8 |
--  |-----+-----|
--  │ 9 8 | 7 6 | 
--  │ 5 4 | 3 2 |  
--  └     |     ┘ 
-- 
-- Supongamos que f = maximo. Como los elementos de la primera ventana son [1,2,5,6],
-- el resultado de f es 6. Al final, el resultado es una matriz de 2x2 (ya que 
-- n=4, v=2, n/v = 2):
--  ┌     ┐ 
--  | 6 8 |
--  | 9 7 |
--  └     ┘
-- 
-- Se pide definir la función (pooling m v f), donde m es la matriz de entrada,
-- v el tamaño de la ventana, y f la función de agregación. El resultado es un
-- Maybe Matriz, ya que si v no es divisor de n, ni mayor o igual que 1, y m 
-- no es cuadrada, entonces devuelve Nothing. Por ejemplo:

type Matriz = Array (Int,Int) Float

mej1,mej2 :: Matriz
mej1 = listArray ((1,1),(4,4)) [1,2,3,4,
                                5,6,7,8,
                                9,8,7,6,
                                5,4,3,2]                               
mej2 = listArray ((1,1),(10,10)) [1..100]

-- > pooling mej1 2 maximum
-- Just (array ((1,1),(2,2)) [((1,1),6.0),((1,2),8.0),((2,1),9.0),((2,2),7.0)])
-- > pooling mej1 2 sum    
-- Just (array ((1,1),(2,2)) [((1,1),14.0),((1,2),22.0),((2,1),26.0),((2,2),18.0)])
-- > pooling mej2 5 minimum
-- Just (array ((1,1),(2,2)) [((1,1),1.0),((1,2),6.0),((2,1),51.0),((2,2),56.0)])
-- > pooling mej2 5 (\xs -> sum xs / (fromIntegral (length xs)))
-- Just (array ((1,1),(2,2)) [((1,1),23.0),((1,2),28.0),((2,1),73.0),((2,2),78.0)])
-- > pooling (listArray ((1,1),(1,2)) [3,4]) 2 maximum
-- Nothing
-- > pooling mej2 3 minimum                                     
-- Nothing
-- > pooling mej2 0 minimum                                     
-- Nothing

-- NOTA: por simplicidad, puedes asumir que los índices de m comienzan en (1,1)


pooling :: Matriz -> Int -> ([Float] -> Float) -> Maybe Matriz
pooling m v f 
    | n1 /= n2 || mod n1 v /= 0 || v <= 0 = Nothing 
    | otherwise = Just $ listArray ((1,1),(n',n')) [ g i j | i <- [0..n'-1], j <- [0..n'-1] ]
  where g i j = f [ m!(x,y) | x <- [i*v+1 .. (i+1)*v], y <- [j*v+1 .. (j+1)*v] ]
        n2 = snd (snd (bounds m))
        n1 = fst (snd (bounds m))
        n' = div n1 v



-- -----------------------------------------------------------------------------
-- Ejercicio 4 (2,5 puntos)
--
-- Años atrás, antes del boom de los datos, no existía un formato de 
-- almacenamiento estandarizado y casi cada programador definía su propio
-- formato de almacenamiento en modo texto. Para poder hacer uso de esos
-- datos históricos en los nuevos algoritmos de aprendizaje automático 
-- es necesario convertir esos formatos a un formato estandar como por
-- ejemplo CSV. 
--
-- En este ejercicio se pide realizar una función que convierta el fichero
-- notas.txt a un formato CSV válido usando como separador de columnas
-- la coma ",". Una vez convertido, guardar el resultado en "notas.csv"
-- y leer con el paquete Text.CSV mostrando por pantalla su contenido.
--
-- Resultado esperado en disco de notas.csv:
--
-- Nombre,Apellidos,Nota
-- David,Martinez Rojas,7.5
-- Juan,Perez Vera,6.4
-- Manuel,Durán Veleño,4.5
-- Miguel,Pereila Rodriguez,3.1
-- Sandra,Sánchez Rojas,6.5 
--
-- Salida por pantalla esperada tras la lectura con Test.CSV:
--
-- "Nombre","Apellidos","Nota"
-- "David","Martinez Rojas","7.5"
-- "Juan","Perez Vera","6.4"
-- "Manuel","Durán Veleño","4.5"
-- "Miguel","Pereila Rodriguez","3.1"
-- "Sandra","Sánchez Rojas","6.5"
--
-- Ayuda: la función intrecalate lleva a cabo la unión de una lista de cadenas
-- usando otra cadena que intercalará entre cada uno de los elementos de la 
-- cadena.
--
-- > intercalate "," ["uno", "dos", "tres"]
-- "uno,dos,tres"



reemplazar :: Eq a => a -> a -> [a] -> [a]
reemplazar c1 c2 = map $ \c -> if c == c1 then c2 else c

recortarInicio :: String -> String
recortarInicio "" = ""
recortarInicio cadena 
 | head cadena == ' ' = recortarInicio (tail cadena)
 | otherwise = cadena
 
recortar :: String -> String 
recortar = recortarInicio . reverse . recortarInicio . reverse

formatearLinea :: String -> String
formatearLinea = (intercalate ",") . (map recortar) . lines . (reemplazar '|' '\n') . init . tail

formatearLineas :: [String] -> String
formatearLineas = (intercalate "\n") . map formatearLinea

convertir :: FilePath -> IO ()
convertir fichero = do
 contenido <- readFile fichero
 let lineas = lines contenido
 let lineasFormateadas = formatearLineas ((lineas!!1):((iterate tail (init lineas)) !! 3))

 writeFile "notas.csv" lineasFormateadas
 
 return ()

main = do
  convertir "notas.txt"
  
  contenido <- readFile "notas.csv"
  let csv = parseCSV "notas.csv" contenido
      filas = case csv of
          (Right lineasCSV) -> lineasCSV
          _ -> []
          
  putStrLn $ printCSV filas
