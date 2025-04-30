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

-- Otra opción, completamente válida, 
-- para modelar a los soldados blancos, es usar DATA.
{-

data SoldadoBlanco = UnSoldadoBlanco {
  nombre :: Nombre,
  habilidades :: [Habilidad]
}

-}
-- Aquí lo modelamos como tupla, así podína tener un ejemplo
-- de como lo usamos apropiadamente, de forma oficial. 

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

-- Otra opción para modelar palo y rol de combate es usar String,
-- para nosotros la utilización de Múltiples Constructores es mejor. 
-- Permite evitar errores humanos de escritura. Al usar Strings, podríamos
-- tener en el sistema "espia" y espía", o "Diamante" y "diamante".
-- Al no tener un consenso de como se escribe, es más fácil confundirse en la escritura. 
-- No solo eso, sino que al crear un Múltiple Constructor evitamos que
-- en la utilización del sistema, alguien diga que un palo es "basto"
-- podría ser un error común, y los strings lo permitirían, los Múltiples
-- Constructores no. 
-- No es un error usar Strings, pero por sus posibels errores y fallas,
-- recomendmos usar Múltiples Constructores. 

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

daliaCentenaria :: SoldadoBlanco
daliaCentenaria = ("daliaCentenaria", ["espada de petalo", "daga espinosa", "lanzador de abono"])

rosaEnvidiosa :: SoldadoBlanco
rosaEnvidiosa = ("rosaEnvidiosa", ["daga espinosa", "escudo rojo pasión", "rifle ak16"])

gatoDeCheshire :: SoldadoBlanco
gatoDeCheshire = ("gatoChesire", ["desaparecetus", "abrete sesamo", "abracadabra"])

sombrerero :: SoldadoBlanco
sombrerero = ("sombrerero", [])

ejercitoReinaBlanca :: FuerzasBlancas
ejercitoReinaBlanca = (daliaCentenaria, gatoDeCheshire)

-- Noten como dalia centenariaa y Gato de Chesire estan modelados como ejemplos,
-- como instancias de soldado blanco, de la misma forma qu rosa envidiosa
-- o sombrerero. NO LOS MODELO como las únicas opciones del ejerciot de la reina blanca.
-- Son ambos soldados blancos, y aunque ahora formna parte de las fuerzas blancas, 
-- en el futuro tal vez no, y debo saber modelarlo correctamente. 
-- Sería un error hacer esto: 

{-

data DaliaCentenaria = UnSoldadoBlanco {
  nombre :: Nombre,
  habilidades :: [armas]
}

-}

-- EN vez del data o las tuplas de Soldado Blanco.
-- Lo mismo si hago esto:
-- type EjercitoBlanco = (daliaCentenaria, gatoDeCheshire)
-- En vez de lo que aparece en esta solucion liberada. 
-- El enunciado dice especificamnete que en el futuro podría cambiar
-- por lo que hay que tenerlo en cuenta al modelar. 

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
sumaHabilidad = calculoComunPersonajes funcionSumaHabilidad

poder :: SoldadoBlanco -> Number
poder = calculoComunPersonajes funcionPoder

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

calculoComunPersonajes :: Number -> SoldadoBlanco -> Number
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

{-
energiaPorCantArmas :: [String] -> Number
energiaPorCantArmas armas
    | length armas <= 3 = length armas * 5
    | otherwise = 3 * 5 - (length armas - 3) * 3

Recordar la forma en la que resolvimos los pinos en el TP anterior! No está mal pero en este caso podría ser más expresiva y declarativa

-}

bonusCantBase :: [Habilidad] -> Number
bonusCantBase habilidades = min (length habilidades) cantBaseHabilidades * puntosBase

multaSobrepeso :: [Habilidad] -> Number
multaSobrepeso habilidades = max (length habilidades - cantBaseHabilidades) 0 * cantMulta 

bonusElemento :: String -> [Habilidad] -> Number
bonusElemento eltoClave habilidades
  | elem eltoClave habilidades = 10
  | otherwise = 0

-- Noten como teniamos la posibilidad de hacer algo así:
{-
tieneEspadaDePetalo :: DaliaCentenaria -> Bool
tieneEspadaDePetalo dalia = "Espada de petalo" `elem` armas dalia

tieneDesaparecetus :: GatoDeCheshire -> Bool
tieneDesaparecetus gato = "Desaparecetus" `elem` hechizos gato
-}

-- Esa solución, aunque es mas declarativa y expresiva, REPITE MUCHO CODIGO. 
-- Recuerden la clase, cada vez que no se reutiliza codigo, muere un gatito. 
-- Se evalua en los examanes por su reutilizacion de codigo, demuestra que 
-- conocen su codigo completo y que pueden pensar criticamnte sobre como pogramar. 
-- Deben estar entrenados para spotear stas instancias donde el enunciado
-- implicitamente, pide exactamente lo mismo en dos lados **muchas veces
-- es trampita para poder evaluar la reutilizacion de codigo**. 
-- Tienen que darse cuenta que tiene espada de petalo y tiene desaparacetus
-- ambos evaluan si se encuentra un string en una lista de strings, mas alla
-- si unas son armas y la otra hechizos. 

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

{-
if nivel > 5 then Explosivo else Guerrero

Aunque no está mal usar if then else, y en otras cursadas encima se lo da en clase, aquí NO lo daremos como tema y NO esperamos que lo usen. Si requieren condicionales, recomendamos usar GUARDAS. 

Lo mismo con otras posibilidades del lenguaje como WHERE, LET, CASE, IN, MAYBE, en esta cursada NO lo vemos y NO lo usamos, NO usarlos en su solución. 

-}

{-
determinarRol :: Palo -> Number -> Estado -> String
determinarRol Corazon _ _ = "Curador"
determinarRol Pica _ _ = "Guerrero"
determinarRol Diamante nivel _
    | nivel > 5 = "Explosivo"
    | otherwise = "Guerrero"
determinarRol Trebol _ estado
    | estado == Activo = "Espía"
    | otherwise = "Guerrero"

otra opción para rol combate.

-}

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

{-
poderMagicoReinaRoja :: [Carta] -> Number
poderMagicoReinaRoja cartas 
    | length cartas == 1 = poderMagicoCarta (head cartas) * 3
    | otherwise = suma de poderes de cartas. 

Esto como vemos en la solución puede resolverse de forma mas clara con PATTERN MATCHING
-}

{-
poderPorEstadoInactivo :: Number
poderPorEstadoInactivo = 0

poderBasePorRolExplosivo :: Number
poderBasePorRolExplosivo = 12

poderBasePorRolGuerrero :: Number
poderBasePorRolGuerrero = 8

poderBasePorRolEspia :: Number
poderBasePorRolEspia = 7
nroARestarleElNivelDeEspia :: Number
nroARestarleElNivelDeEspia = 13
menosPoderPorSerEspiaMuyBueno :: Number
menosPoderPorSerEspiaMuyBueno = 30

poderBasePorRolCurador :: Number
poderBasePorRolCurador = 0.5


no siempre es correcto colocar MUCHAS constantes para hacer mas expresivo el codigo, hay que hacer un balance
-}

-- --------------- Infiltracion Encubierta

-- Función principal: infiltrar una carta en el ejército de la Reina Roja
infiltrarCarta :: CartaMagica -> Number -> FuerzasRojas -> FuerzasRojas
infiltrarCarta cartaInfiltrada n ejercito =
  init ejercito ++ [transformarCarta cartaInfiltrada n] ++ [last ejercito]

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

-- Como se va a cambiar el palo y el nivel en VARIAS partes del programa
-- entonces, abstraigo su logica en una funcion auxiliar. 
-- Permite ser mas declarativos, expresivos y reutilizr codigo. 

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
prepararCarta carta = transformar (prepararCarta carta)

prepararCarta :: CartaMagica -> CartaMagica
prepararCarta carta
  | estado carta == Activa = cambiarNivel carta (nivel carta + 2)
  | otherwise = carta

transformar :: CartaMagica -> CartaMagica
transformar carta
  | poderMagicoCarta carta > 120 = cambiarPalo carta Diamante
  | poderMagicoCarta carta < 20  = cambiarPalo carta Corazon
  | otherwise = carta

-- --------------- Test de Equilibrio

formacionBalanceada :: [CartaMagica] -> Bool
formacionBalanceada ejercito = esPar ejercito && rolesDif ejercito

esPar :: [CartaMagica] -> Bool
esPar ejercito = even (length ejercito)

-- Recordamos que esto es un incorrecto uso de guardas:
{-
esCantidadDeCartasPar :: [Carta] -> Bool
esCantidadDeCartasPar cartas
    | even (length cartas) = True
    | otherwise= False
-}


rolesDif :: [CartaMagica] -> Bool
rolesDif ejercito = 
  rolEnCombate (head ejercito) /= rolEnCombate (last ejercito)

{-
verificarNuevoPoderMagicoYAsignarPalo :: Number -> CartaMagica -> CartaMagica

intercambiarRolesDeEjercitoReinaBlanca :: EjercitoReinaBlanca -> EjercitoReinaBlanca

nroPrimerasLetrasDeNombreCoincidentes :: Number

Generar código expresivo no siempr significa agregar mas palabras, sino que a una mirada rapida o semi rapida, l gente entienda el codigo. En estos casos, la cantidad de palabras es contraproducente, lo mismo con constantes de mas
-}
  
