{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Library where
import PdePreludat

doble nro = nro *2

type Bien = (String,Number)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Number, cantidadDeHijos :: Number, bienes :: [Bien] } deriving Show
type Ciudad = [Ciudadano]

data Gobierno = UnGobierno {periodo :: [Number],  medidas :: [ Ciudadano->Ciudadano ]}

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

springfield = [homero, burns, frink, krabappel] 

diferenciaDePatrimonio :: Ciudad -> Number
diferenciaDePatrimonio ciudad = (maximum.patrimonios) ciudad - (minimum.patrimonios) ciudad

patrimonios :: Ciudad -> [Number]
patrimonios ciudad = map  calcularPatrimonio ciudad

calcularPatrimonio :: Ciudadano -> Number
calcularPatrimonio ciudadano = foldl (\sem (_, valor) -> sem + valor)  (sueldo ciudadano) (bienes ciudadano)

{-*Spec Library Spec> diferenciaDePatrimonio springfield
2011000 -}

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama ciudadano = (any esAutoAltaGama . bienes) ciudadano

esAutoAltaGama :: Bien -> Bool
esAutoAltaGama ("auto", valor) = valor > 100000
esAutoAltaGama _  = False

---3
type Medida = Ciudadano -> Ciudadano

-- a
auh :: Medida
auh ciudadano  = nuevoCiudadano ((<0).calcularPatrimonio) ((1000*).cantidadDeHijos) ciudadano


nuevoCiudadano :: (Ciudadano -> Bool) -> (Ciudadano -> Number) -> Ciudadano -> Ciudadano
nuevoCiudadano f g ciudadano | f ciudadano = modificarSueldo (g ciudadano) ciudadano
                             | otherwise = ciudadano

modificarSueldo :: Number -> Ciudadano -> Ciudadano
modificarSueldo cantidad ciudadano = ciudadano { sueldo = sueldo ciudadano + cantidad}

-- b
impuestoGanancias :: Number -> Medida
impuestoGanancias minimo ciudadano = nuevoCiudadano ((>minimo).sueldo) (diferencia minimo) ciudadano

diferencia :: Number -> Ciudadano -> Number
diferencia minimo ciudadano = (minimo - sueldo ciudadano) * 0.3

-- c
impuestoAltaGama :: Medida
impuestoAltaGama ciudadano = nuevoCiudadano tieneAutoAltaGama montoADisminuir ciudadano

montoADisminuir :: Ciudadano -> Number
montoADisminuir ciudadano = ((-0.1)*).snd.head .(filter esAutoAltaGama).bienes $ ciudadano

--d
negociarSueldoProfesion :: String -> Number -> Medida
negociarSueldoProfesion unaProfesion porcentaje ciudadano = nuevoCiudadano ((==unaProfesion).profesion)  (calcularMonto porcentaje)  ciudadano

calcularMonto :: Number -> Ciudadano -> Number
calcularMonto porcentaje ciudadano =  (sueldo ciudadano * porcentaje) /100

-- 4
gobiernoA = UnGobierno [1999..2003] [impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10, negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh]

gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40]

gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidas (medidas gobierno))  ciudad

aplicarMedidas :: [Medida] -> Ciudadano -> Ciudadano
aplicarMedidas medidas ciudadano = foldl  (flip ($))  ciudadano   medidas


gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = foldl (flip ($))  ciudad (replicate (length.periodo $ gobierno)(gobernarUnAño gobierno))

distribuyoRiqueza :: Gobierno -> Ciudad -> Bool
distribuyoRiqueza gobierno ciudad = diferenciaDePatrimonio ciudad > (diferenciaDePatrimonio.gobernarPeriodoCompleto gobierno) ciudad

kane :: Ciudadano
kane = UnCiudadano "Empresario" 100000 0 [("Rosebud", valor) | valor <- [5, 10..]]