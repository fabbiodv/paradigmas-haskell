-- Pattern Matching

signo :: Integer -> Integer
signo nro
  | nro == 0 = 0
  | nro < 0 = -1
  | otherwise = 1

suma :: Integer -> (Integer -> Integer) -- Currificacion
suma nro otroNum = nro + otroNum

suma' :: (Integer, Integer) -> Integer
suma' (nro, otroNum) = nro + otroNum

siguiente :: Integer -> Integer
siguiente nro = nro + 1

-- Composicion de funciones
-- :t -> es el type
-- :t (siguiente . (suma 8))
-- (siguiente . suma 8) :: Integer -> Integer

-- Expresiones Lambda
-- >(\nro -> nro + 1) 10
-- 11

type Radio = Double
type Area = Double
area :: Radio -> Area

area radio = pi * radio ^ 2

area Rectangulo :: Base -> Altura -> Area
area Rectangulo base altura = base * altura

area::Figura -> Area
area (circulo radio) = pi * radio ^ 2
area (Rectangulo base altura) = bas * alt
area (Cuadrado lado) = lado * lado

data Bool = True | False
data colores = Rojo | Azul | Amarillo

-- Ejercicio 1
cuadruple :: Integer -> Integer
cuadruple nro = (doble . doble) nro
-- doble (doble nro)

-- Ejercicio 2
esMayorA::Integer -> Bool
esMayorA nro = ( (>10) . doble . siguiente . suma 2) nro 
--             ((>10). doble . siguiente . (+2)) nro

-- Ejercicio 3
triple nro = (\x -> 3*x) nro 
siguiente nro = (\x -> x + 1) nro
suma nro otroNum = (\x y -> x + y) nro otroNum
sumarDos nro = (\n -> n + 2) nro 

-- Ejercicio 4
data Empleado = Comun { sueldoBasico::Double, nombre::String} | Jefe { sueldoBasico::Double, cantACargo::Int, nombre::String }

type sueldo = Double
sueldo::Empleado->sueldo
sueldo(comun basico _ ) = basico
sueldo(jefe basico catACargo _ ) = (basico + plus) cantACargo
plus::Int->Double
plus cant = cant * 1000

sonia = Jefe 15000 3 "Sonia"
pedro = comun 10000 "Pedro"

