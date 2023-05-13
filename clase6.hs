-- Ejercicio 2) Dada una lista de tuplas, sacar la cantidad de elementos utilizando fold y foldr
--Main>cantidadDeElementos[(8,6),(5,5),(5,6),(7,8)]

-- foldl::(a->b->a) -> a -> [b] -> a
-- foldl _ z [] = z
-- foldl f z (x:cola) = foldl f (fxz) cola

cantidadDeElementos lista = foldl(\sem _ -> sem + 1) 0 lista
cantidadDeElementos' lista = foldl contar 0 lista

contar sem _ = sem + 1

cantidadDeElementos'' lista = foldr (\ _ sem -> sem + 1 ) 0 lista

-- flip f x y = f y x
cantidadDeElementos'' lista = foldr ( flip contar ) 0 lista


-- 3) Dada una lista de pares (empleado, gasto), conocer el empleado mas gastrador usando foldl y foldr

masGastador :: [(String, Integer)] -> ( String, Integer )

masGastador (cab : cola) = foldl mayorGasto cab cola

mayorGasto::( String, Integer ) -> ( String, Integer ) -> ( String, Integer )
mayorGasto ( nom, valor ) ( otroNom, otroValor ) 
  | valor > otroValor = (nom, valor)
  | otherwise = (otroNom, otroValor)

mayorGasto' persona otraPersona 
  | snd persona > snd otraPersona = persona
  | otherwise = otraPersona

-- 4)Dada una lista de (empleado, gasto), conocer el gasto total usando foldl y foldr

monto::[(String, Integer)] -> Integer
monto lista = foldl sumarGasto 0 lista

sumarGasto::Integer -> (String, Integer) -> Integer
sumarGasto sem (_ , monto) = sem + monto

-- monto' lista 0 = foldr (\ ( _ , n) sem -> sem + m) 0 lista

-- 5) Completar con lo que corresponda para:

-- >foldl(\sem f -> sem ) 2[(3+), (+2), (5+)]
-- >foldl ( flip ($)) 2[(3+), (+2), (5+)]
-- > foldr (\f sem -> f sem) 2[(3+), (+2), (5+)]
-- > foldr ($) 2[(3+), (+2), (5+)]


-- 6)  Dada una lista de proyectos
-- Determine una función que permita conocer el máximo proyecto según. Revolverlo usando foldl y foldr.
-- a) La inversión inicial
-- b) El nro de profesionales.
-- c) La cantidad de palabras del proyecto.
-- Muestre por cada caso ejemplos de invocación y respuesta.
type Nombre  = String
type InversionInicial = Integer
type Profesionales = [String]
data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show
proyectos = [
  Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], 
  Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"],
  Proy "ventaChurros" 1000 ["cocinero"]]

maximoProyectoSegun :: [Proyecto] -> (Proyecto -> Integer) -> Proyecto
maximoProyectoSegun listaProyectos f = foldl (maxSegun f) (head listaProyectos)(tail listaProyectos)

maxSegun::(Proyecto -> Integer) -> Proyecto -> Proyecto -> Proyecto
maxSegun f unProyecto otroProyecto
  | f unProyecto > f otroProyecto = unProyecto
  | otherwise = otroProyecto

-- a)
-- maximoProyectoSegun proyectos inversionInicial
--Proy "red social de arte"  20000 ["ing. en sistemas", "contador"]

-- b)
-- maximoProyectoSegun proyectos (length.profesionales) 

-- c) 
-- maximoProyectoSegun proyectos (length.words.nombre) 


--------------- TIPOS GENÉRICOS ---------------
--------------- POLIMORFISMO PARAMÉTRICO ---------------

-- Num TypeClass
-- Int, Integer, Float, Double
--(+), (*), (-), abs

-- Ord
-- Int, Integer, Float, Double (a, b), [a],
-- Char String Bool
-- (>), (>=), (<), (<=)

max::(Ord a) => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

-- Eq
-- (==), (/=)
-- Int, Integer, Float, Double (a, b), [a], Char, String, Bool

-- elem x lista = any (==x) lista
-- >elem (+1)[(2*), (+1), (5+)]
-- Error

-- Show
-- show
-- Int, Integer, Float, Double (a, b), [a], Char, String, Bool

-- Ejercicio 1
p::(a -> Bool) -> [a] -> a
p n l = (head.filter n) l

-- Ejercicio 2
f::(Ord b) => (a -> b) -> [(a, a)] -> Bool
f x y = (x.fst.head) y > (x.snd.head) y
-- >f (+1) [(2,1) (3,4)]
-- TRUE