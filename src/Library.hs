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

type Brujo = (Nombre, [Hechizo])
type Capitan = (Nombre, [Arma])
type FuerzasBlancas = (Capitan, Brujo)

-- Se podria plantear tambien:
-- type Habilidad = String 
-- type SoldadoBlanco = [Habilidad]
-- type FuerzasBlancas = (SoldadoBlanco, SoldadoBlanco)

-- --------------- Definicion de Tipos

type Nivel = Number
type Arma = String
type Hechizo = String
type Nombre = String

data EstadoActividad = Activa | Inactiva
  deriving Show

data Palo = Corazon | Pica | Diamante | Trebol
  deriving Show

data RolCombate = Curador | Guerrero | Explosivo | Espia
  deriving Show

-- --------------- Ejemplos

-- Ejemplos de cartas
carta1 :: CartaMagica
carta1 = CartaMagica 5 Corazon Activa

carta2 :: CartaMagica
carta2 = CartaMagica 3 Pica Inactiva

carta3 :: CartaMagica
carta3 = CartaMagica 10 Diamante Activa

carta4 :: CartaMagica
carta4 = CartaMagica 7 Trebol Inactiva

ejercitoReinaRoja :: [CartaMagica]
ejercitoReinaRoja = [carta1, carta2, carta3, carta4]

daliaCentenaria :: Capitan
daliaCentenaria = ["espada de petalo", "daga espinosa", "lanzador de abono"]

rosaEnvidiosa :: Capitan
rosaEnvidiosa = ["daga espinosa", "escudo rojo pasión", "rifle ak16"]

gatoDeCheshire :: Brujo
gatoDeCheshire = ["desaparecetus", "abrete sesamo", "abracadabra"]

sombrerero :: [String]
sombrerero = []

-- Si se elige la otra opción, aca encontrariamos
-- sombrereo :: SoldadoBlanco
-- sombrerero = []

fuerzasReinaBlanca :: FuerzasBlancas
fuerzasReinaBlanca = (daliaCentenaria, gatoDeCheshire)

-- -----------------------------------------------------------------------------
-- --------------- PARTE II: PREPARANDO CAMPO DE BATALLA ---------------------------------

-- --------------- Energia Total

-- A CAMBIAR

energiaTotalReinaBlanca :: FuerzasBlancas -> Number
energiaTotalReinaBlanca (capitan, brujo) = sumaHabilidad brujo + poder capitan

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad = calculo

{-
El lenguaje nos permite simplificar, como si fuera una ecuacion matematica,
aquellos valores que encontramos de ambos lados. 

Al igual que una ecuacion, no se puede sacar cualquier cosa, solo podemos sacar
los parametros (nunca funciones) que estén "sueltas".

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad personaje = energia personaje
-}


energia :: SoldadoBlanco -> Number -> Number
energia (nombre, lista) tipoPersonaje
  | null lista = length nombre * 5
  | otherwise = 50 + bonusPersonaje lista tipoPersonaje

bonusPersonaje :: [Arma] -> Number -> Number
bonusPersonaje armas 1 = bonusElemento "espada de petalo" armas + bonusArmas armas
bonusPersonaje hechizos 0 = bonusElemento "desaparacetus" hechizos





cantBaseEspadas :: Number
cantBaseEspadas = 3

puntosBase :: Number
puntosBase = 5

cantMulta :: Number
cantMulta = 3


{-
  energiaTotalReinaBlanca :: FuerzasBlancas -> Number
energiaTotalReinaBlanca (capitan, brujo) = energiaCapitan capitan + energiaBrujo brujo

energiaCapitan :: Capitan -> Number
energiaCapitan (nombres, armas)
  | null armas = length nombre * 5
  | otherwise = 50 + bonusElemento "espada de petalo" armas + bonusArmas armas

energiaBrujo :: Brujo -> Float
energiaBrujo (Brujo nombre hechizos _)
  | null hechizos = letrasNombre nombre * 5
  | otherwise = 50 + bonusElemento "desaparacetus" hechizos
-}
  
bonusArmas :: [Arma] -> Number
bonusArmas armas = bonusCantBase armas - multaSobrepeso armas

bonusCantBase :: [Arma] -> Number
bonusCantBase armas = min (length armas) cantBaseEspadas * puntosBase

multaSobrepeso :: [Arma] -> Number
sobrepeso armas = max (length armas - cantBaseEspadas) 0 * cantMulta

bonusElemento :: String -> [String] -> Number
bonusEspada eltoClave armas
  | elem eltoClave armas = 10
  | otherwise = 0

-- --------------- Poder Mágico

-- Determina el rol en combate de una carta
rolCombate :: CartaMagica -> RolCombate
rolCombate (UnaCartaMagica nivel Corazon _) = Curador
rolCombate (UnaCartaMagica nivel Pica _) = Guerrero
rolCombate (UnaCartaMagica nivel Diamante _) 
  | nivel > 5 = Explosivo
  | otherwise = Guerrero
rolCombate (CartaMagica _ Trebol estadoAct)
  | estadoAct == Activa = Espia
  | otherwise = Guerrero

-- Poder mágico individual de una carta
poderMagicoCarta :: CartaMagica -> Float
poderMagicoCarta (CartaMagica _ _ Inactiva) = 0
poderMagicoCarta carta
  | rolCombate carta == Explosivo = nivel carta * 12
  | rolCombate carta == Guerrero = nivel carta * 8 + bonusDiamante (palo carta)
  | rolCombate carta == Espia  = poderEspia (nivel carta)
  | rolCombate carta == Curador = 0.5

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
poderMagicoReinaRoja :: [CartaMagica] -> Float
poderMagicoReinaRoja [] = 0
poderMagicoReinaRoja [carta] = 3 * poderMagicoCarta carta
poderMagicoReinaRoja cartas = poderMagicoCarta (head cartas) + poderMagicoCarta (cartas !! 1) + poderMagicoCarta (last cartas)


-- --------------- Infiltracion Encubierta

-- Función principal: infiltrar una carta en el ejército de la Reina Roja
infiltrarCarta :: CartaMagica -> Number -> [CartaMagica] -> [CartaMagica]
infiltrarCarta carta n ejercito =
  tomarInicio ejercito ++ [transformarCarta carta n] ++ [last ejercito]

-- Toma todas las cartas menos la última
tomarInicio :: [CartaMagica] -> [CartaMagica]
tomarInicio ejercito = take (length ejercito - 1) ejercito

-- Transformación según número secreto n
transformarCarta :: CartaMagica -> Number -> CartaMagica
-- A COMPLETAR

-- -----------------------------------------------------------------------------
-- --------------- PARTE III: ARCO DE ENTRENAMIENTO ---------------------------------

-- --------------- UTN: Universidad de Trucos y Nigromancia
-- A CAMBIAR ?

aprenderHechizo :: Brujo -> String -> Brujo
aprenderHechizo brujo hechizo
  | puedeAprender brujo hechizo = (fst brujo, snd brujo ++ [hechizo])
  | otherwise = brujo

-- Verifica si el brujo puede aprender el hechizo
puedeAprender :: Brujo -> String -> Bool
puedeAprender brujo hechizo 
  | primerasTresLetras (fst brujo) == primerasTresLetras hechizo = True
  | length (snd brujo) > 100 = True
  | hechizoYaDominado hechizo brujo = False
  | otherwise = False

primerasTresLetras :: String -> String
primerasTresLetras = take 3

hechizoYaDominado :: String -> Brujo -> Bool
hechizoYaDominado hechizo brujo = hechizo `elem` (snd brujo)

-- --------------- Pre-Batalla

-- Preparamos una carta para la batalla según su nivel y poder mágico
prepararCarta :: CartaMagica -> CartaMagica
prepararCarta carta
  | estado carta == Activa = transformar carta{ nivel = nivel carta + 2 }
  | otherwise = carta

transformar :: CartaMagica -> CartaMagica
transformar carta
  | poderMagico carta > 120 = carta { palo = Diamante }
  | poderMagico carta < 20  = carta { palo = Corazon }
  | otherwise = carta

-- --------------- Test de Equilibrio

formacionEquilibrada :: [CartaMagica] -> Bool
formacionEquilibrada ejercito = esPar ejercito && rolesDif ejercito

esPar :: [CartaMagica] -> Bool
esPar ejercito = even length ejercito

rolesDif :: [CartaMagica] -> Bool
rolesDif ejercito = 
  rolCombate (head ejercito) /= rolCombate (last ejercito)
