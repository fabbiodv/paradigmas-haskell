and' :: Bool -> Bool -> Bool
and' valor1 valor2 | not valor1 = False
  | not valor2 = False
  | otherwise = True

and'' :: Bool -> Bool -> Bool
and'' valor otroValor 
  | valor = otroValor
  | otherwise = False

and''' :: Bool -> Bool -> Bool
and''' True otroValor = otroValor
and''' _ otroValor = False

-- even verifica si el numero es par

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' False otroValor = otroValor
or'' _ _ = True

-- or''' v1 v2 
--   | not v1 = v2
--   | otherwise = True

-- or'''' True _ = True
-- or''' _ True = True
-- or''' _ _ = False

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota, otraNota, otraNota') = nota `max` (otraNota `max` otraNota')

cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)