-- Programación Declarativa - Tercera Convocatoria 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Diciembre                                      14 de Diciembre de 2021
-- ----------------------------------------------------------------------
-- Lee el fichero instrucciones.pdf adjunto en la carpeta material para 
-- ver cómo entregar el examen.
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------

-- Con estas importaciones debería ser suficiente, pero puedes usar otras
-- si lo necesitas
import Data.Char
import Data.Array
import Data.List
import Control.Exception (catch, SomeException)

-- ---------------------------------------------------------------------
-- Ejercicio 1. (3 puntos)
-- ---------------------------------------------------------------------
-- La lista [7, 10, 12, 1, 2, 4] no está ordenada, pero si consideramos
-- las listas que se pueden formar cíclicamente a partir de cada 
-- elemento, obtenemos:
--    [7, 10, 12, 1, 2, 4]
--    [10, 12, 1, 2, 4, 7]
--    [12, 1, 2, 4, 7, 10]
--    [1, 2, 4, 7, 10, 12]  ** ordenada **
--    [2, 4, 7, 10, 12, 1]
--    [4, 7, 10, 12, 1, 2]
-- Se observa que una de ellas está ordenada.
-- 
-- Se dice que una lista [x(0), ..., x(n)] es cíclicamente ordenable
-- si existe un índice i tal que la lista
--    [x(i), x(i+1), ..., x(n), x(0), ..., x(i-1)]
-- está ordenada. 
-- 
-- Definir la función
--    ciclicamenteOrdenable :: Ord a => [a] -> Bool
-- tal que (ciclicamenteOrdenable xs) se verifica si xs es una lista
-- cíclicamente ordenable. Por ejemplo,
--    ciclicamenteOrdenable [7,10,12,1,2,4]  ==  True
--    ciclicamenteOrdenable [7,20,12,1,2,4]  ==  False
-- de las siguientes formas:
--   a) (1 punto) usando listas por comprensión
--   b) (1 punto) usando recursión
--   c) (1 punto) usando orden superior (pista: pueden ser útiles las
--      funciones tails e inits).
-- Nota: la comprobación de si una lista está ordenada puede ser común
-- a las tres implementaciones.
-- ---------------------------------------------------------------------

-- Como dice la nota anterior, podemos usarla en los tres apartados
ordenada :: Ord a => [a] -> Bool
ordenada xs = xs == sort xs

-- a) con comprensión
ciclicamenteOrdenableC :: Ord a => [a] -> Bool
ciclicamenteOrdenableC xs = or [ordenada (rota i xs) | i <- [0..length xs]]
  where rota n xs = (drop n xs) ++ (take n xs)

-- b) con recursión
ciclicamenteOrdenableR :: Ord a => [a] -> Bool
ciclicamenteOrdenableR xs = ordenada xs || aux (rota xs)
  where aux ys | ys == xs = False
               | otherwise = ordenada ys || aux (rota ys)
        rota (x:xs) = xs ++ [x]

-- c) con orden superior
ciclicamenteOrdenableS :: Ord a => [a] -> Bool
ciclicamenteOrdenableS xs =
  any ordenada (zipWith (++) (tails xs) (inits xs))

-- ---------------------------------------------------------------------


-- ----------------------------------------------------------------------
-- Ejercicio 2. (2,5 puntos)
-- ---------------------------------------------------------------------
-- Representaremos expresiones boolenas con el siguiente tipo de dato 
-- algebraico:

-- data Expr = V String      -- variable
--           | N Expr        -- operador NO (NOT)
--           | O Expr Expr   -- operador O (OR)   (MODIFICAR)          
--           | Y Expr Expr   -- operador Y (AND)  (MODIFICAR)          
--           deriving (Eq, Show)

-- ejb1, ejb2 :: Expr 
-- ejb1 = Y (O (N (V "P")) (V "Q")) (O (N (V "P")) (N (V "R")))
-- ejb2 = Y (O (N (V "P")) (V "Q")) 
--          (Y  (O (N (V "P")) (N (V "R"))) 
--              (O (V "P") (O (N (V "Q")) (V "R"))))

-- Ejercicio 2.1. (1 punto) Redefine el tipo de las expresiones O e Y para
-- que puedan tener asociadas un número arbitrario de expresiones. Hazlo
-- comentando la definición anterior y volviendo a definirla debajo del
-- enunciado. Modifica también, a mano, los ejemplos de acuerdo a la nueva
-- definición, uniendo las operaciones O (e Y) seguidas dentro de una sola
-- operación.

-- Usando listas podemos asignar un número arbitrario de expresiones
data Expr = V String      -- variable
          | N Expr        -- operador NO (NOT)
          -- | O Expr Expr   -- operador O (OR)   (MODIFICAR)
          | O [Expr]
          -- | Y Expr Expr   -- operador Y (AND)  (MODIFICAR)
          | Y [Expr]
          deriving (Eq, Show)

ejb1, ejb2 :: Expr 
ejb1 = Y [O [N (V "P"), V "Q"], O [N (V "P"), N (V "R")]]
ejb2 = Y [O [N (V "P"), V "Q"],
          O [N (V "P"), N (V "R")],
          O [V "P", N (V "Q"), V "R"]]

-- Ejercicio 2.2. (1,5 puntos) Define la función
--     evalua :: Expr -> [(String,Bool)] -> Maybe Bool
-- tal que (evalua e vs) devuelva simplemente el booleano correspondiente
-- al evaluar toda la expresión v con las asignaciones de verdad de
-- las variables según la lista vs, o nada si no todas las variables
-- en la expresión tienen asignaciones de verdad.
-- λ> evalua ejb1 [("P",True),("Q",False),("R",False)]
-- Just False
-- λ> evalua ejb2 [("P",False),("Q",False),("R",True)]
-- Just True
-- λ> evalua ejb2 [("P",False),("R",True)]
-- Nothing
-- ----------------------------------------------------------------------

evalua :: Expr -> [(String,Bool)] -> Maybe Bool
evalua e vs
  | contenida vars (map fst vs) = Just (evalua' e vs)
  | otherwise = Nothing  -- si no están todas la variables definidas
  where vars = variables e

-- El conjunto de variables de la expresión
variables :: Expr -> [String]
variables (V v) = [v]
variables (N e) = variables e
variables (Y es) = nub (concatMap variables es)
variables (O es) = nub (concatMap variables es)

-- Si una lista está contenida en otra
contenida :: Eq a => [a] -> [a] -> Bool
contenida [] ys = True
contenida (x:xs) ys = elem x ys && contenida xs ys

-- Esta función hace la evaluación ya teniendo en cuenta
-- que todas las variables están definidas
evalua' :: Expr -> [(String,Bool)] -> Bool
evalua' (V v) vs = head [b | (v',b) <- vs, v' == v ]
evalua' (N e) vs = not (evalua' e vs)
evalua' (Y es) vs = and [evalua' e vs | e <- es]
evalua' (O es) vs = or [evalua' e vs | e <- es]

-- ---------------------------------------------------------------------

 
-- ----------------------------------------------------------------------
-- Ejercicio 3 (2 puntos)
-- ----------------------------------------------------------------------
-- Define las funciones (normalizaFilas p) y (normalizaColumnas p). Ambas
-- reciben una matriz de dos dimensiones, y devuelven otra matriz donde
-- los valores están normalizados por filas o normalizados por columnas
-- (respectivamente). Es decir, cada valor debe ser dividido por el valor
-- absoluto de la suma de su fila (o columna). Las filas y columnas cuya
-- suma sea cero no se normalizan (observa qué sucede con mej2). Por ejemplo,
-- λ> elems (normalizaFilas mej1)
-- [0.1,0.2,0.3,0.4,
--  0.2,0.4,0.2,0.2,
--  0.5,0.2,0.2,0.1,
--  0.4,0.1,0.4,0.1]
-- λ> elems (normalizaFilas mej2)
-- [0.14285715,0.0,0.2857143,0.5714286,
--  0.3846154,0.0,0.0,0.61538464,
--  0.0,0.0,0.0,0.0,
--  -7.0,0.0,3.0,4.0]
-- λ> elems (normalizaColumnas mej2)
-- [1.0,0.0,0.4,0.25,
--  5.0,0.0,0.0,0.5,
--  0.0,0.0,0.0,0.0,
-- -7.0,0.0,0.6,0.25]
-- λ> elems $ normalizaColumnas $ normalizaFilas mej1
-- [8.333333e-2,0.22222222,0.27272728,0.49999997,
--  0.16666666,0.44444445,0.18181819,0.24999999,
--  0.41666666,0.22222222,0.18181819,0.12499999,
--  0.3333333,0.11111111,0.36363637,0.12499999]
-- ----------------------------------------------------------------------
mej1, mej2 :: Array (Int,Int) Float 
mej1 = listArray ((1,1),(4,4)) [1, 2, 3, 4, 
                                1, 2, 1, 1, 
                                5, 2, 2, 1, 
                                4, 1, 4, 1]
mej2 = listArray ((1,1),(4,4)) [1, 0, 2, 4, 
                                5, 0, 0, 8, 
                                0, 0, 0, 0, 
                                -7, 0, 3, 4]                                


normalizaFilas :: Array (Int,Int) Float -> Array (Int,Int) Float
normalizaFilas p = array ((1,1),(n,m)) [((i,j), f i j) | i <- [1..n], j <- [1..m]]
  where n = fst(snd(bounds p))
        m = snd(snd(bounds p))
        vf = listArray (1,n) [abs (sum [p!(i,j)|j<-[1..m]]) | i <- [1..n] ]
        f i j | vf!i == 0 = p!(i,j)
              | otherwise = p!(i,j) / vf!i  -- dividir por la suma de la fila

normalizaColumnas :: Array (Int,Int) Float -> Array (Int,Int) Float
normalizaColumnas p = array ((1,1),(n,m)) [((i,j), f i j) | i <- [1..n], j <- [1..m]]
  where n = fst(snd(bounds p))
        m = snd(snd(bounds p))
        vc = listArray (1,m) [abs (sum [p!(i,j)|i<-[1..n]]) | j <- [1..m] ]
        f i j | vc!j == 0 = p!(i,j)
              | otherwise = p!(i,j) / vc!j  -- dividir por la suma de la columna

-- ----------------------------------------------------------------------


-- ----------------------------------------------------------------------
-- Ejercicio 4 (2,5 puntos)
-- ----------------------------------------------------------------------
-- Se pide reproducir el juego de la calculadora humana de Saber y Ganar. 
-- Aquí, al concursante se le da dos números enteros y una operación 
-- (+,-,*,/), y debe indicar el resultado (otro número entero). Tiene
-- intentos ilimitados, y cuando acierta pasa a otra operación, donde el
-- primer operando es el último valor calculado, y el segundo es el que
-- nuevo que se le indique. Por ejemplo:
--     3 por 5 = 15
--     más 7 = 22
--     entre 2 = 11,   
--     más 45 = 56 ...
-- Escribe un programa interactivo que reproduzca este juego, siguiendo
-- estos pasos:
--  * primero debe cargar una secuencia de operaciones desde un fichero
--    cuyo nombre se pida al empezar. Un ejemplo del contenido de este
--    fichero es el siguiente (puedes asumir que es correcto):
--         2 + 3 * 5 / 5 + 865 - 34 * 2
--  * si el fichero no existe o está vacío, debe solicitarlo de nuevo.
--  * una vez cargado el contenido, debe mostrar al usuario la primera
--    operación. Éste debe introducir su resultado, si no es correcto
--    se le debe pedir de nuevo que introduzca la solución.
--  * cuando sea correcto, se pasa a la siguiente operación. Se mostrará
--    tan solo una operación y un número.
--  * si el usuario escribe "fin" en vez de un resultado, se debe mostrar
--    la puntuación conseguida, que es igual al número de operaciones
--    resueltas, y terminar.
--  * si el usuario escribe "res" en vez de un resultado, el programa
--    debe mostrar el resultado de la última operación y pasar a la
--    siguiente. Esta operación resuelta no contabiliza para la solución.
--  * si ya no hay más operaciones que hacer, se muestra la puntuación
--    total y termina.
-- Un ejemplo de traza del juego:
-- λ> main
-- Introduce fichero de texto: sec.txt
-- sec.txt: openFile: does not exist (No such file or directory)
-- error fichero no existe o vacio
-- Introduce fichero de texto: secuencia.txt
-- 2 + 3: 5
-- Correcto!
-- * 5: 25
-- Correcto!
-- - 3: 21
-- Incorrecto! Prueba de nuevo
-- - 3: res
-- La solucion es 22
-- * 9: fin
-- Fin. Puntuacion 2

-- El main lee de fichero e inicia el juego
main :: IO ()
main = do
  putStr "Introduce fichero de texto: "
  filename <- getLine
  input <- catch (readFile filename)
                   (\err -> print (err::SomeException) >> return "")
  if (length input == 0) then do
        putStrLn "error fichero no existe o vacio"
        main
    else do
        let css = words input
        putStr $ (head css) ++ " "
        juego (tail css) (read (head css)) 0

-- El juego va procesando las palabras de entrada, que son los
-- operadores y los números
juego :: [String] -> Int -> Int -> IO ()
juego [] _ p = do
  putStrLn $ "Terminado con " ++ show p ++ " puntos."
juego [_] _ p = do
  putStrLn $ "Error: secuencia incorrecta, esto no deberia suceder."
juego ass@(cs1:cs2:css) v p = do
  let s = solucion cs1 v (read cs2)  -- solucion de la operación
  putStr $ cs1 ++ " " ++ cs2 ++ ": "
  ns <- getLine
  case ns of
    "fin" -> putStrLn $ "Fin. Puntuacion " ++ (show p)
    "res" -> do
      putStrLn $ "La solucion es " ++ (show s)
      juego css s p
    _ ->
      if (not (all isDigit ns)) then do -- Este if es opcional
        putStrLn $ "Introduzca un numero, fin o res"
        juego ass v p
      else do
        let n = read ns  -- respuesta del usuario
        if (n /= s) then do
          putStrLn "Incorrecto! Prueba de nuevo"
          juego ass v p
         else do
          putStrLn "Correcto!"
          juego css s (p+1)

-- Calcular la solución de la operación
solucion :: String -> Int -> Int -> Int
solucion "+" a b = a+b
solucion "*" a b = a*b
solucion "/" a b = div a b
solucion "-" a b = a-b
solucion _ _ _ = error "no es posible"

-- Otra solución, más elegante, haciendo una función de string a función
solucion' :: String -> Int -> Int -> Int
solucion' cs a b = (readFuncion cs) a b

readFuncion :: String -> (Int -> Int -> Int)
readFuncion "+" = (+)
readFuncion "*" = (*)
readFuncion "/" = (div)
readFuncion "-" = (-)
readFuncion _ = error "no es posible"
  
