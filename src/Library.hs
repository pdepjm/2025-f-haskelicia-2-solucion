module Library where
import PdePreludat

-- -----------------------------------------------------------------------------
-- --------------- PARTE I: MODELANDO EL REINO ---------------------------------

-- --------------- Dominio

-- Carta Mágica: nivel, palo y estado de actividad.
data CartaMagica = UnaCartaMagica {
  nivel :: Nivel,
  palo :: Palo,
  estado :: EstadoActividad
} deriving Show

-- Solemos tratar de generar una diferenciación entre 
-- Tipo de Dato y Constructor, haciendo que el tipo de 
-- dato sea simple "CartaMagica" y el contructor 
-- "UnaCartaMagica".

type FuerzasRojas = [CartaMagica]

type SoldadoBlanco = (Nombre, [Habilidad])
type Brujo = SoldadoBlanco
type Capitan = SoldadoBlanco
type FuerzasBlancas = (Capitan, Brujo)

-- --------------- Definicion de Tipos

type Nivel = Number
type Nombre = String
type Habilidad = String

data EstadoActividad = Activa | Inactiva
  deriving (Eq, Show)

data Palo = Corazon | Pica | Diamante | Trebol
  deriving (Eq, Show)

data RolCombate = Curador | Guerrero | Explosivo | Espia
  deriving (Eq, Show)

-- --------------- Ejemplos

-- Ejemplos de cartas
carta1 :: CartaMagica
carta1 = UnaCartaMagica 5 Corazon Activa

carta2 :: CartaMagica
carta2 = UnaCartaMagica 3 Pica Inactiva

carta3 :: CartaMagica
carta3 = UnaCartaMagica 10 Diamante Activa

carta4 :: CartaMagica
carta4 = UnaCartaMagica 7 Trebol Inactiva

ejercitoReinaRoja :: FuerzasRojas
ejercitoReinaRoja = [carta1, carta2, carta3, carta4]

daliaCentenaria :: Capitan
daliaCentenaria = ("daliaCentenaria", ["espada de petalo", "daga espinosa", "lanzador de abono"])

rosaEnvidiosa :: Capitan
rosaEnvidiosa = ("rosaEnvidiosa", ["daga espinosa", "escudo rojo pasión", "rifle ak16"])

gatoDeCheshire :: Brujo
gatoDeCheshire = ("gatoChesire", ["desaparecetus", "abrete sesamo", "abracadabra"])

sombrerero :: SoldadoBlanco
sombrerero = ("sombrerero", [])

ejercitoReinaBlanca :: FuerzasBlancas
ejercitoReinaBlanca = (daliaCentenaria, gatoDeCheshire)

-- -----------------------------------------------------------------------------
-- --------------- PARTE II: PREPARANDO CAMPO DE BATALLA ---------------------------------

-- --------------- Energia Total

funcionSumaHabilidad :: Number
funcionSumaHabilidad = 1

funcionPoder :: Number
funcionPoder = 0

energiaTotalReinaBlanca :: FuerzasBlancas -> Number
energiaTotalReinaBlanca (capitan, brujo) = sumaHabilidad brujo + poder capitan

sumaHabilidad :: SoldadoBlanco -> Number
sumaHabilidad soldado = calculoComunPersonajes soldado funcionSumaHabilidad

poder :: SoldadoBlanco -> Number
poder soldado = calculoComunPersonajes soldado funcionPoder

{-
El lenguaje nos permite simplificar, como si fuera una ecuacion matematica,
aquellos valores que encontramos de ambos lados. 

Al igual que una ecuacion, no se puede sacar cualquier cosa, solo podemos sacar
los parametros (nunca funciones) que estén "sueltas".

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad personaje = calculoComunPersonajes personaje

Se vuelve

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad = calculoComunPersonajes

-}

calculoComunPersonajes :: SoldadoBlanco -> Number -> Number
calculoComunPersonajes (nombre, habilidades) tipoFuncion
  | null habilidades = length nombre * 5
  | otherwise = 50 + bonusPorFuncion habilidades tipoFuncion

bonusPorFuncion:: [Habilidad] -> Number -> Number
bonusPorFuncion habilidades tipoFuncion
  | tipoFuncion == funcionPoder = bonusElemento "espada de petalo" habilidades + bonusCantHabilidades habilidades
  | tipoFuncion == funcionSumaHabilidad = bonusElemento "desaparacetus" habilidades


cantBaseHabilidades :: Number
cantBaseHabilidades = 3

puntosBase :: Number
puntosBase = 5

cantMulta :: Number
cantMulta = 3
  
bonusCantHabilidades :: [Habilidad] -> Number
bonusCantHabilidades habilidades = bonusCantBase habilidades - multaSobrepeso habilidades

bonusCantBase :: [Habilidad] -> Number
bonusCantBase habilidades = min (length habilidades) cantBaseHabilidades * puntosBase

multaSobrepeso :: [Habilidad] -> Number
multaSobrepeso habilidades = max (length habilidades - cantBaseHabilidades) 0 * cantMulta 

bonusElemento :: String -> [Habilidad] -> Number
bonusElemento eltoClave habilidades
  | elem eltoClave habilidades = 10
  | otherwise = 0

-- --------------- Poder Mágico

-- Determina el rol en combate de una carta
rolEnCombate :: CartaMagica -> RolCombate
rolEnCombate carta
  | palo carta == Corazon = Curador
  | palo carta == Pica = Guerrero
  | palo carta == Diamante = rolParaDiamante carta
  | otherwise = rolParaTrebol carta

rolParaDiamante :: CartaMagica -> RolCombate
rolParaDiamante carta
  | nivel carta > 5 = Explosivo
  | otherwise = Guerrero

rolParaTrebol :: CartaMagica -> RolCombate
rolParaTrebol carta
  | estado carta == Activa = Espia
  | otherwise = Guerrero

-- Poder mágico individual de una carta
poderMagicoCarta :: CartaMagica -> Number
poderMagicoCarta carta
  | estado carta == Inactiva = 0
  | rolEnCombate carta == Explosivo = nivel carta * 12
  | rolEnCombate carta == Guerrero = nivel carta * 8 + bonusDiamante (palo carta)
  | rolEnCombate carta == Espia  = poderEspia (nivel carta)
  | rolEnCombate carta == Curador = 0.5

bonusDiamante :: Palo -> Number
bonusDiamante palo
  | palo == Diamante = 35
  | otherwise = 0

poderEspia :: Number -> Number
poderEspia nivel
  | poderEspiaBase nivel > 100 = poderEspiaBase nivel - 30
  | otherwise = poderEspiaBase nivel

poderEspiaBase :: Number -> Number
poderEspiaBase nivel = (13 - nivel) * 7

-- Poder mágico total de la Reina Roja
poderMagicoReinaRoja :: [CartaMagica] -> Number
poderMagicoReinaRoja [] = 0
poderMagicoReinaRoja [carta] = 3 * poderMagicoCarta carta
poderMagicoReinaRoja cartas = poderMagicoCarta (head cartas) + poderMagicoCarta (cartas !! 1) + poderMagicoCarta (last cartas)


-- --------------- Infiltracion Encubierta

-- Función principal: infiltrar una carta en el ejército de la Reina Roja
infiltrarCarta :: CartaMagica -> Number -> FuerzasRojas -> FuerzasRojas
infiltrarCarta cartaInfiltrada n ejercito =
  tomarInicio ejercito ++ [transformarCarta cartaInfiltrada n] ++ [last ejercito]

-- Toma todas las cartas menos la última
tomarInicio :: [CartaMagica] -> [CartaMagica]
tomarInicio ejercito = take (length ejercito - 1) ejercito

-- Transformación según número secreto n
transformarCarta :: CartaMagica -> Number -> CartaMagica
transformarCarta cartaInfiltrada n
  | esDivisible n 4 = cambiarPalo (cambiarNivel cartaInfiltrada n) Diamante
  | n == 33 = cambiarPalo cartaInfiltrada Corazon
  | esDivisible n 7 = cambiarPalo cartaInfiltrada Pica
  | otherwise = cartaInfiltrada{estado = Inactiva}

esDivisible :: Number -> Number -> Bool
esDivisible dividendo divisor = mod dividendo divisor == 0

cambiarPalo :: CartaMagica -> Palo -> CartaMagica
cambiarPalo carta nuevoPalo = carta{palo = nuevoPalo}

cambiarNivel :: CartaMagica -> Number -> CartaMagica
cambiarNivel carta nuevoNivel = carta{nivel = nuevoNivel}

-- Aunque uno podría estar tentado a hacer una función 
-- cambiaProp :: CartaMagica -> a -> b -> CartaMagica
-- cambiarProp carta prop valor = carta{a = valor}
-- Lamentablemente no se puede !
-- Lo que sí, en proximas clases veremos formas muy cool de
-- resolver otro tipo de cosas parecidas. 

-- -----------------------------------------------------------------------------
-- --------------- PARTE III: ARCO DE ENTRENAMIENTO ---------------------------------

-- --------------- UTN: Universidad de Trucos y Nigromancia

-- Primera Funcion

nombre :: SoldadoBlanco -> Nombre
nombre = fst

habilidades :: SoldadoBlanco -> [Habilidad]
habilidades = snd

aprenderHechizo :: SoldadoBlanco -> Habilidad -> SoldadoBlanco
aprenderHechizo soldado habilidad
  | puedeAprender soldado habilidad = (nombre soldado, habilidades soldado ++ [habilidad])
  | otherwise = soldado

puedeAprender :: SoldadoBlanco -> Habilidad  -> Bool
puedeAprender soldado habilidad 
  | habilidadYaDominada habilidad soldado  = False
  | primerasTresLetrasIguales (nombre soldado) habilidad = True
  | length (habilidades soldado) > 100 = True
  | otherwise = False

primerasTresLetrasIguales :: String -> String -> Bool
primerasTresLetrasIguales p1 p2 = primerasTresLetras p1 == primerasTresLetras p2

primerasTresLetras :: String -> String
primerasTresLetras = take 3

habilidadYaDominada :: String -> Brujo -> Bool
habilidadYaDominada habilidad soldado = habilidad `elem` snd soldado

-- Segunda Funcion
intercambiarRolSeguidores :: FuerzasBlancas -> FuerzasBlancas
intercambiarRolSeguidores (capitan, brujo) = (brujo, capitan)

-- --------------- Pre-Batalla

-- Preparamos una carta para la batalla según su nivel y poder mágico
prepararCarta :: CartaMagica -> CartaMagica
prepararCarta carta = transformar (actualizarNivel carta)

--prepararCarta :: CartaMagica -> CartaMagica
prepararCarta carta
  | estado carta == Activa = cambiarNivel carta (nivel carta + 2)
  | otherwise = carta

transformar :: CartaMagica -> CartaMagica
transformar carta
  | poderMagicoCarta carta > 120 = cambiarPalo carta Diamante
  | poderMagicoCarta carta < 20  = cambiarPalo carta Corazon
  | otherwise = carta


actualizarNivel:: CartaMagica -> CartaMagica
actualizarNivel carta = carta
--TODO

-- --------------- Test de Equilibrio

formacionBalanceada :: [CartaMagica] -> Bool
formacionBalanceada ejercito = esPar ejercito && rolesDif ejercito

esPar :: [CartaMagica] -> Bool
esPar ejercito = even (length ejercito)

rolesDif :: [CartaMagica] -> Bool
rolesDif ejercito = 
  rolEnCombate (head ejercito) /= rolEnCombate (last ejercito)
