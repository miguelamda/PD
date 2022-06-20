-- -----------------------------------------------------------------------------
-- Programación Declarativa 2021/22
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2                                                 14 de Enero de 2022
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Array
import Control.Exception (catch, SomeException)
import PilaConListas

-- -----------------------------------------------------------------------------
-- Ejercicio 1.1 (2,5 puntos)
-- Dada la siguiente definición de árbol binario

data Arbol a = H a | N (Arbol a) a (Arbol a)
  deriving Show

-- Define la función (dropWhileArbol a p), tal que reciba un árbol a, una
-- propiedad p, y devuelva una lista de árboles (es decir, un bosque). Este
-- bosque lo conforman aquellos subárboles del árbol a que queden tras eliminar
-- nodos, empezando por la raíz, mientras cumplan con la propiedad p. El
-- comportamiento es similar al dropWhile para listas, se dejará de eliminar
-- nodos cuando se encuentre alguno que no cumpla con la propiedad. Por ejemplo: 
-- 
--                3
--              /   \
--             4     5     dropWhile (>2)
--            / \   / \    ------------->      
--           2   1 3  -2                      2      1    -2
--          / \                              / \
--         0  -3                            0  -3
--
-- Más ejemplos:
-- λ> dropWhileArbol (>2) ejArbol
-- [N (H 0) 2 (H (-3)),H 1,H (-2)]
-- λ> dropWhileArbol (>3) ejArbol
-- [N (N (N (H 0) 2 (H (-3))) 4 (H 1)) 3 (N (H 3) 5 (H (-2)))]
-- λ> dropWhileArbol odd ejArbol
-- [N (N (H 0) 2 (H (-3))) 4 (H 1),H (-2)]
-- -----------------------------------------------------------------------------

ejArbol :: Arbol Int
ejArbol = N (N (N (H 0) 2 (H (-3))) 4 (H 1)) 3 (N (H 3) 5 (H (-2)))

dropWhileArbol :: (a -> Bool) -> Arbol a -> [Arbol a]
dropWhileArbol = undefined

-- -----------------------------------------------------------------------------
-- Ejercicio 1.2 (1 punto)
-- En la imagen adjunta al examen hay un ejemplo de un árbol rojo-negro
-- (Red-Black tree, en inglés). En estos árboles, los nodos pueden ser Black
-- (negros) o Red (rojos). Los nodos hoja no contienen valores. Se cumple que
-- la raíz y las hojas son Black, y todo nodo Red siempre debe tener como hijos
-- nodos Black. Se pide:
--   a) Definir un tipo algebraico para representar este tipo de árbol. Hazlo
--   parametrizado de tal forma que el tipo de los valores en el árbol pueda
--   ser cualquiera.
--   b) Usando tu nuevo tipo algebraico, define el ejemplo que hay en la imagen
--   adjunta.

-- Define el tipo RedBlackTree a continuación

-- Define el ejemplo ejArbolRN a continuación

-- -----------------------------------------------------------------------------


-- -----------------------------------------------------------------------------
-- Ejercicio 2.1 (2,5 puntos)
-- Softmax es una operación que se utiliza comúnmente en machine learning. Esta
-- función recibe un vector z como entrada, de dimensión K, y devuelve otro
-- vector de dimensión K donde los valores están normalizados. En concreto, el
-- valor en la posición j se calcula como:
--
--                e^(z[j]) / (sumatorio{k=1..K} e^(z[k]))
--
-- Por ejemplo, el vector [0.1,0.2,0.9,0.5] se transformaría a
-- [0.17174646,0.18980919,0.38222876,0.2562156]. En concreto, el valor en la
-- primera posición es: e^0.1 / (e^0.1+e^0.2+e^0.9+e^0.5) = 0.17174646, en la
-- segunda posición es: e^0.2 / (e^0.1+e^0.2+e^0.9+e^0.5) = 0.18980919, ...

-- Define la función (softmax p) tal que reciba una matriz p de dos
-- dimensiones, y devuelva el resultado de aplicar la operación softmax a cada
-- columna de la matriz.
-- Nota 1: en Haskell, el número e se puede aproximar con (exp 1)
-- Nota 2: puedes asumir que el índice de la matriz empieza por 1
-- Por ejemplo,
-- λ> elems $ softmax ejm1
-- [0.17135403,0.20753783,0.23625901,0.24026075,
--  0.3450646, 0.2534873, 0.35245705,0.3584269,
--  0.17135403,0.22936477,0.17502499,0.16105159,
--  0.31222737,0.30961007,0.23625901,0.24026075]
-- λ> elems $ softmax ejm2
-- [2.1639727e-3, 0.2, 1.7809076e-2,
--  0.118148886,  0.2, 2.4101965e-3,
--  7.960811e-4,  0.2, 0.9723425,
--  0.8730087,    0.2, 6.551593e-3,
--  5.8822874e-3, 0.2, 8.8666176e-4]

ejm1, ejm2 :: Array (Int,Int) Float
ejm1 = listArray ((1,1),(4,4)) [0.1, 0.2, 0.3, 0.4, 
                                0.8, 0.4, 0.7, 0.8, 
                                0.1, 0.3, 0.0, 0.0, 
                                0.7, 0.6, 0.3, 0.4]
ejm2 = listArray ((1,1),(5,3)) [1, 0, 3,
                                5, 0, 1, 
                                0, 0, 7,
                                7, 0, 2,
                                2, 0, 0]                                

softmax :: Array (Int,Int) Float ->  Array (Int,Int) Float
softmax = undefined

-- -----------------------------------------------------------------------------
-- Ejercicio 2.2 (1 punto)
-- Define la propiedad (normalizada p) tal que reciba una matriz p y compruebe
-- que sus columnas están normalizadas; es decir, la suma de sus valores es
-- aproximadamente 1 (con un error de 0.001). Por ejemplo,
-- λ> normalizada $ softmax ejm1
-- True
-- λ> normalizada ejm1
-- False
-- λ> normalizada $ softmax ejm2
-- True
-- λ> normalizada ejm2
-- False

normalizada :: Array (Int,Int) Float -> Bool
normalizada = undefined

-- ----------------------------------------------------------------------


-- ----------------------------------------------------------------------
-- Ejercicio 3. (3 puntos) Define un programa que haga lo siguiente:
--   1. Pedir al usuario un nombre de fichero
--   2. Si el fichero no existe o está vacío, mostrar un mensaje de error y
--      volver a repetir.
--   3. Si el fichero existe o no está vacío, cargar su contenido y procesarlo
--      con un pequeño parser.
--   4. El parser tan solo indicará si el fichero es correcto o no. Lo que hará
--      será comprobar que los paréntesis "()" y las llaves "{}" están
--      debidamente cerrados. Hay que detectar que ningún paréntesis se ha
--      cerrado cuando había una llave abierta, y viceversa. Además, todos los 
--      paréntesis y llaves deben estar debidamente cerrados antes de
--      finalizar el fichero. Por ejemplo:
--       - es correcto: {()}, ({()}), {({})}
--       - es incorrecto: ({)}, ({(})), {, ((({})
--      Para ello, DEBES usar una pila. Cuando proceses la cadena con el
--      contenido del fichero, si te encuentras con un '(' o '{', añádelo a la
--      pila. Si te encuentras un ')' o un '}', comprueba que corresponde a lo
--      que haya en la cima de la pila. Al finalizar, la pila debe estar vacía.
--   5. si hay un error de este tipo, el programa debe escribir por
--      pantalla "incorrecto", mientras que si no hay error debe escribir
--      "correcto".
-- Por ejemplo:
-- λ> main
-- Indica nombre de fichero: fich1.cpp
-- Correcto
-- λ> main
-- Indica nombre de fichero: fich2.cpp
-- Incorrecto
-- λ> main
-- Indica nombre de fichero: fich.cpp
-- fich.cpp: openFile: does not exist (No such file or directory)
-- error fichero no existe
-- Indica nombre de fichero: fich3.cpp
-- Incorrecto
-- ----------------------------------------------------------------------

main :: IO ()
main = undefined
  
-- ----------------------------------------------------------------------

