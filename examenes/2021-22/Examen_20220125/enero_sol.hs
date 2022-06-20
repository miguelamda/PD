-- -----------------------------------------------------------------------------
-- Programación Declarativa 2021/22
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Enero (primera convocatoria)                              25 de Enero de 2022
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Array

-- -----------------------------------------------------------------------------
-- Ejercicio 1.1 (2,5 puntos)
-- Dada dos cadenas de texto as y bs, define la función (convertir as bs) que
-- convierta la cadena as en la bs. Para ello se consideran tan solo dos 
-- operaciones posibles:
--    1. eliminar la última letra de la cadena as. 
--    2. agregar una letra al final de la cadena as.
-- La función debe devolver la lista de operaciones necesarias para realizar la 
-- conversión. Por ejemplo:
--
-- > convertir "" "hola"
-- ["agregar h","agregar o","agregar l","agregar a"]
-- > convertir "hola que tal" "hola"
-- ["eliminar","eliminar","eliminar","eliminar","eliminar","eliminar","eliminar","eliminar"]
-- > convertir "holitas" "hola"
-- ["eliminar","eliminar","eliminar","eliminar","agregar a"]
-- > convertir "hotla" "hola"
-- ["eliminar","eliminar","eliminar","agregar l","agregar a"
-- > convertir "aaa" "ba"    
-- ["eliminar","eliminar","eliminar","agregar b","agregar a"]

-- Una Solución
ultimos :: Int -> [a] -> [a]
ultimos n = reverse . take n . reverse

convertir :: String -> String -> [String]  
convertir so sd = convertir_aux so sd [] []  

convertir_aux :: [Char] -> [Char] -> [Char] -> [[Char]] -> [[Char]]
convertir_aux [] [] sdi op = op ++ ["agregar " ++ [l] | l <- sdi]
convertir_aux [] sdd [] op = op ++ ["agregar " ++ [l] | l <- sdd]
convertir_aux so sdd sdi op 
   | lso > lsdd = convertir_aux (take lsdd so) sdd [] (take (lso - lsdd) (repeat "eliminar"))
   | so == sdd  = convertir_aux [] [] sdi op
   | lso < lsdd = convertir_aux so (take lso sdd) (ultimos (lsdd - lso) sdd) [] 
   | otherwise  = convertir_aux (init so) (init sdd) ((last sdd):sdi) (op ++ ["eliminar"])
   where 
       lsdd = length sdd 
       lso = length so

-- Otra solución
convertir' :: String -> String -> [String]
convertir' as bs 
  | as == bs = []
  | esPrefijo as bs = ("agregar " ++ c) : convertir' (as++c) bs
  | otherwise = "eliminar" : convertir' as' bs
  where c = take 1 (drop (length as) bs)
        as' = take (length as - 1) as
        esPrefijo = isPrefixOf     -- Definido en Data.List
        --esPrefijo [] _ = True    -- También se puede definir a mano
        --esPrefijo xs ys =  xs == take (length xs) ys

-- Ejercicio 1.2 (0,5 puntos)
-- Comprueba con quickCheck que cualquier cadena as se puede convertir en bs, es
-- decir, que la secuencia de operaciones nunca es vacía, a no ser que as sea
-- igual que bs.

prop_convierte :: String -> String -> Property 
prop_convierte as bs = as /= bs ==> not (null (convertir as bs))

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Ejercicio 2 (2,5 puntos)
-- Usaremos el tipo de dato algebraico de árbol con valores solo en los nodos:

data Arbol a = H | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- Un árbol A está subcontenido en otro árbol B si los elementos de A aparecen
-- en B respetando el orden de jerarquía. Esto es, para cada nodo n del árbol A, 
-- se cumple lo siguiente:
--   * El valor del nodo n está en el árbol B. Sea m el nodo con ese valor en B.
--   * El subárbol izquierdo del nodo n está subcontenido en el subárbol 
--     izquierdo del nodo m (ídem para el subárbol derecho). 
--   * Un árbol que es un nodo hoja siempre está subcontenido en otro árbol.
-- Por ejemplo, supongamos los siguientes árboles:
--  a1:        a2:            a3:        a4:
--      3            2           3           3
--     / \          / \         / \         / \
--    2   1        3   1       4   1       2   4
--                            /           / \
--                           2           6   0
--                                      /     \      
--                                     3       1
--
-- El árbol a1 está subcontenido en a3 y el árbol a2 está subcontenido en a4. 
-- Pero a1 no está subcontenido en a4 ni en a2 (comparten raíz, y el subárbol
-- izquierdo está subcontenido, pero no el subárbol derecho), y tampoco a2 está 
-- en a3 (el nodo 2 está en a3, pero en a3 no tiene subárboles).
-- Define la función (subcontenido a1 a2) tal que compruebe que el árbol a1
-- está subcontenido en a2.

a1,a2,a3,a4 :: Arbol Int 
a1 = N 3 (N 2 H H) (N 1 H H) 
a2 = N 2 (N 3 H H) (N 1 H H)
a3 = N 3 (N 4 (N 2 H H) H) (N 1 H H)
a4 = N 3 (N 2 (N 6 (N 3 H H) H) (N 0 H (N 1 H H))) (N 4 H H)

-- > subcontenido a1 a3
-- True
-- > subcontenido a1 a4
-- False
-- > subcontenido a2 a4
-- True
-- > subcontenido a2 a3
-- False
-- > subcontenido a2 a1
-- False
-- > subcontenido a1 a2
-- False
 
subcontenido :: Eq a => Arbol a -> Arbol a -> Bool
subcontenido H _ = True
subcontenido a1@(N n i1 d1) (N m i2 d2) 
  | n == m = subcontenido i1 i2 && subcontenido d1 d2
  | otherwise = subcontenido a1 i2 || subcontenido a1 d2
subcontenido _ _ = False

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Ejercicio 3 (2 puntos)
-- Define la función (pliegaMatriz m f), donde m es una matriz de dos dimensiones,
-- y f es una función de dos argumentos, (f x y). Plegar una matriz es el resultado
-- de aplicar la función f, elemento a elemento, a la primera columna con la última
-- columna, la segunda con la penúltima, la tercera con la antepenúltima ...
-- la penúltima con la segunda y la última con la primera. Si la columna tiene un
-- número impar de columnas, f se aplica a la columna central con ella misma. Por
-- ejemplo, supongamos la siguiente matriz con un número impar de columnas y f = +:
--  ┌           ┐            ┌                                         ┐        ┌             ┐
--  │ 1 2 3 4 5 |   pliega   | (f 1 5) (f 2 4) (f 3 3) (f 4 2) (f 5 1) |  f=+   | 6  6 6 6 6  |
--  │ 2 3 1 6 8 |  ------->  | (f 2 8) (f 3 6) (f 1 1) (f 6 3) (f 8 2) |  --->  | 10 9 2 9 10 |
--  └           ┘            └                                         ┘        └             ┘
--  Otro ejemplo con un número par de columnas y f = ^ :             
--  ┌         ┐            ┌                                 ┐        ┌          ┐
--  | 3 4 2 1 |   pliega   | (f 3 1) (f 4 2) (f 2 4) (f 1 3) |  f=^   | 3 16 8 1 |
--  | 2 3 3 2 |  ------->  | (f 2 3) (f 3 3) (f 3 3) (f 2 2) |  --->  | 4 9  9 4 |
--  | 1 2 3 4 |            | (f 1 4) (f 2 3) (f 3 2) (f 4 1) |        | 1 8  9 4 |
--  └         ┘            └                                 ┘        └          ┘                                                     
-- NOTA: se conseguirá la nota al completo si la función acepta cualquier rango de 
-- índices para las columnas; es decir, no siempre tienen por qué empezar por el 1, 
-- también por el 0, como mej3. Si no lo consigues pero lo haces funcionar para
-- rangos que comiencen por 1 (como mej1 y mej2), obtendrás la mitad de la nota en el
-- ejercicio.
--
mej1,mej2,mej3 :: Array (Int,Int) Int
mej1 = listArray ((1,1),(2,5)) [1,2,3,4,5,
                               2,3,1,6,8]
mej2 = listArray ((1,1),(3,4)) [3,4,2,1,
                               2,3,3,2,
                               1,2,3,4]                               
mej3 = listArray ((0,0),(2,5)) [5,6,3,3,2,2,
                               5,2,3,3,0,2,
                               5,2,3,3,2,2]

-- > elems $ pliegaMatriz (+) mej1
-- [6, 6,6,6,6,
--  10,9,2,9,10]
-- > elems $ pliegaMatriz (+) mej2
-- [4,6,6,4,
--  4,6,6,4,
--  5,5,5,5]
-- > elems $ pliegaMatriz (^) mej2
-- [3,16,16,1,
--  4,27,27,4,
--  1,8, 9, 4]
-- *Main> elems $ pliegaMatriz (*) mej3
-- [10,12,9,9,12,10,
--  10,0, 9,9,0, 10,
--  10,4, 9,9,4, 10]

pliegaMatriz f m = listArray (bounds m) [ g i j | (i,j) <- indices m]
  where g i j = f (m!(i,j)) (m!(i,c2-j+c1))
        c2 = snd (snd (bounds m))
        c1 = snd (fst(bounds m))

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Ejercicio 4 (2,5 puntos)
-- Se pide implementar un menú desplegable en ASCII. Para ello se deberá imprimir 
-- por pantalla una primera línea correspondiente a las opciones del menú principal
--  (Archivo, Editar, Salir). Esta primera línea debe ir con el fondo azul. Las
-- opciones "Archivo" y "Editar" tienen asociados un submenu. Cuando se selecciona
-- una de estas opciones, el submenu correspondiente se debe mostrar debajo de ella.
-- Las opciones de menú se seleccionan mediante una letra que está resaltada en 
-- rojo. Para imprimir por pantalla en color, tanto las letras como el fondo, se
-- deben usar los códigos de escape aportados. Por ejemplo, si queremos imprimir en
-- color rojo un texto, imprimimos por pantalla el código es escape rojo, seguidamente 
-- el texto y por último el código de escape blanco para volver a color habitual.
-- Prueba el siguiente ejemplo
--
-- > putStrLn (rojo ++ "Hola" ++ blanco ++ " mundo")   
--
-- También se propocionar una función para limpiar la pantalla y una lista con la 
-- estructura de menú. Las tuplas (Int, String), representan el índice de la letra a 
-- resaltar en rojo y el texto de la opción de menú o submenú. Cada opción de menú puede
-- tener asociado una lista de opciones de submenu. En el caso de "Salir", esta lista 
-- está vacia, ya que no tiene submenu asociado.
--
-- Nota: ver el video con la funcionalidad esperada a implementar.

-- Esta función limpia la pantalla
limpiar = putStr "\ESC[2J"

-- Códigos de escape de color
rojo = "\ESC[31m" 
blanco = "\ESC[37m"
fondo_azul = "\ESC[44m"
fondo_negro = "\ESC[40m"


menu :: [((Int, String), [(Int, String)])]
menu = [((1, "Archivo"), [(1, "Cargar"), (1, "Guardar")]), ((1, "Editar"), [(1, "Rehacer"), (1, "Copiar"), (1, "Pegar")]), ((1, "Salir"), [] )]

-- Código del alumno...

colorea :: String -> String -> Int -> String
colorea color opcion pos = concat [if pos == i then color ++ (c:blanco) else [c] | (c, i) <- zip opcion [1..] ]  

espacios n = replicate n ' '

muestra_submenu :: [((Int, String), [(Int, String)])] -> Int -> IO()  
muestra_submenu menu i = do
    let sm = snd (menu!!i)
    let submenu_pos = 1 + sum (take i [1 + (length opcion) | ((_, opcion), _) <- menu])
    sequence_ [putStrLn ((espacios submenu_pos) ++ (colorea rojo opcion pos)) | (pos, opcion) <- sm] 
    putStrLn "" 
        
muestra_menu menu = do
    limpiar
    let opciones = concat [((colorea rojo opcion pos) ++ " ") | ((pos, opcion), _) <- menu] 
    putStrLn (fondo_azul ++ opciones ++ (espacios (157 - length opciones)) ++ fondo_negro)

gestiona_menu submenu letra = do
    if submenu /= (-1) then do   
        muestra_menu menu
        muestra_submenu menu submenu
    else do
        muestra_menu menu
        putStrLn ""

    if letra /= "" then
        putStrLn ("Ultima letra pulsada: " ++ letra)
    else
        putStr ""

    putStrLn "Pulse una letra de opción de menú"
    --c <- getChar   -- en Linux
    c <- fmap head getLine      -- en Windows
    let o = toUpper c
    limpiar
    
    case o of
        'A' -> do 
            gestiona_menu 0 "A"
        'E' -> gestiona_menu 1 "E"
        'S' -> return ()
        _   -> gestiona_menu (-1) [o] 


main = do
    limpiar
    gestiona_menu (-1) ""

-- -----------------------------------------------------------------------------