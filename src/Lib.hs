module Lib where
import Text.Show.Functions

laVerdad = True

type Ejercicio = Minutos -> Gimnasta -> Gimnasta

data Gimnasta = Gimnasta {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    coefTonificacion :: Float
    } deriving(Show)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0

--Punto 1

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not.estaObeso) gimnasta && coefTonificacion gimnasta > 5

estaObeso :: Gimnasta -> Bool
estaObeso = (>100).peso

--Punto 2

quemarCalorias :: Gimnasta -> Float -> Gimnasta
quemarCalorias gimnasta calorias | estaObeso gimnasta = bajarPeso gimnasta (calorias/150)
                                 | (not.estaObeso) gimnasta && edad gimnasta > 30 && calorias > 200 = bajarPeso gimnasta 1
                                 | otherwise = bajarPeso gimnasta ((calorias/).(peso gimnasta*).edad $ gimnasta)

bajarPeso :: Gimnasta -> Float -> Gimnasta
bajarPeso gimnasta pesoABajar = gimnasta{peso= peso gimnasta - pesoABajar}

--Punto 3
type Minutos = Float
type PesoPesas = Float
type Inclinacion = Float

caminarEnCinta :: Ejercicio
caminarEnCinta minutos = flip quemarCalorias (1*5*minutos)

ejercicioEnCinta :: Ejercicio
ejercicioEnCinta minutos = flip quemarCalorias (1*promedioVelocidad minutos*minutos)

promedioVelocidad :: Float -> Float
promedioVelocidad minutos = (6+(6+minutos/5))/2

pesas :: PesoPesas -> Ejercicio
pesas peso minutos gimnasta | minutos > 10 = tonificacion (peso/10) gimnasta
                            | otherwise = gimnasta

tonificacion :: Float -> Gimnasta -> Gimnasta
tonificacion loTonificado gimnasta = gimnasta{coefTonificacion = coefTonificacion gimnasta + loTonificado}

colina :: Inclinacion -> Ejercicio
colina inclinacion minutos = flip quemarCalorias (2*minutos*inclinacion)

montania :: Inclinacion -> Ejercicio
montania inclinacion minutos = tonificacion 1 . colina (inclinacion+3) (minutos/2) . colina inclinacion (minutos/2)

--Punto 4

--a
--Con Recursividad
--Lib Lib> hacerRutina laRutina pancho  
--Gimnasta {nombre = "Francisco", edad = 40.0, peso = 91.399994, coefTonificacion = 7.0}

data Rutina = Rutina {
    nombreRutina:: String,
    duracion:: Minutos,
    ejercicios::[Ejercicio]
}deriving(Show)

hacerRutina :: Rutina -> Gimnasta -> Gimnasta
hacerRutina rutina = hacerListaDeEjercicios (ejercicios rutina) (duracionDecadaEjercicio (duracion rutina) (ejercicios rutina))

hacerListaDeEjercicios :: [Ejercicio] -> Minutos -> Gimnasta -> Gimnasta
hacerListaDeEjercicios [] _ gimnasta = gimnasta
hacerListaDeEjercicios (ejercicio : ejercicios) duracionDeCadaEjercicio gimnasta =
     hacerListaDeEjercicios ejercicios duracionDeCadaEjercicio (ejercicio duracionDeCadaEjercicio gimnasta)

duracionDecadaEjercicio :: Float -> [Ejercicio] -> Float
duracionDecadaEjercicio duracionTotal = (duracionTotal /).fromIntegral.length 

laRutina = Rutina "LaRutina" 120 [caminarEnCinta, ejercicioEnCinta, pesas 50, colina 30, montania 30]

--Con fold
--Lib Lib> hacerRutina' laRutina pancho
--Gimnasta {nombre = "Francisco", edad = 40.0, peso = 98.94569, coefTonificacion = 7.0}

hacerRutina' :: Rutina -> Gimnasta -> Gimnasta
hacerRutina' rutina gimnasta =
    foldl (hacerEjercicio (duracionDecadaEjercicio (duracion rutina) (ejercicios rutina))) gimnasta (ejercicios rutina) 

hacerEjercicio :: Minutos -> Gimnasta -> Ejercicio -> Gimnasta 
hacerEjercicio min gimnasta ejercicio = ejercicio min gimnasta

--b
type Resumen = (String, Float, Float)

resumenRutina :: Rutina -> Gimnasta -> Resumen
resumenRutina rutina gimnasta = (nombreRutina rutina, pesoPerdido rutina gimnasta, tonificacionGanada rutina gimnasta)

pesoPerdido :: Rutina -> Gimnasta -> Float
pesoPerdido rutina gimnasta = peso gimnasta - (peso.hacerRutina rutina) gimnasta

tonificacionGanada :: Rutina -> Gimnasta -> Float
tonificacionGanada rutina gimnasta = (coefTonificacion.hacerRutina rutina) gimnasta - coefTonificacion gimnasta

--Punto 5

llegaSaludable :: Gimnasta -> [Rutina] -> [Resumen]
llegaSaludable gimnasta = map (flip resumenRutina gimnasta) .filter (saludableDespuesDeUnaRutina gimnasta)

saludableDespuesDeUnaRutina :: Gimnasta -> Rutina -> Bool
saludableDespuesDeUnaRutina gimnasta = estaSaludable.flip hacerRutina' gimnasta