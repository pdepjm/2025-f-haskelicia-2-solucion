module Library where
import PdePreludat

-- -----------------------------------------------------------------------------
-- --------------- PARTE I: MODELANDO EL REINO ---------------------------------

-- --------------- Dominio

-- Carta Mágica: nivel, palo y estado de actividad.
data CartaMagica = UnaCartaMagica {
  nivel :: Number,
  palo :: Palo,
  estado :: Bool
} deriving Show

-- Para mayor claridad, es importante distinguir entre el tipo de dato y su constructor.
-- En este caso, el tipo de dato es "CartaMagica", mientras que el constructor es "UnaCartaMagica".

type FuerzasRojas = [CartaMagica]

type SoldadoBlanco = (Nombre, [Habilidad])
type Brujo = SoldadoBlanco
type Capitan = SoldadoBlanco
type FuerzasBlancas = (Capitan, Brujo)

-- Como alternativa, también podríamos usar `data` para modelar a los soldados blancos.
-- El código a continuación muestra una posible definición usando `data`. Sin embargo, 
-- hemos optado por usar tuplas en este caso para mostrar cómo utilizarlas correctamente.

{-
data SoldadoBlanco = UnSoldadoBlanco {
  nombre :: Nombre,
  habilidades :: [Habilidad]
}
-}

-- Las tuplas son un tipo de dato básico, anónimo, donde todo está por hacerse.
-- Por eso, idealmente no deberiamos tener entidades importantes del dominio modelados 
-- con ellas, sino con Data, que son como tuplas, pero podemos darle nombres personalizados, 
-- se generan automaticamente funciones para acceder a sus componentes, 
-- y otras facilidades por tratarse de nuevos tipos de datos 

-- --------------- Definicion de Tipos

type Nombre = String
type Caracteristica = String
type Palo = String
type RolCombate = String

-- Por ahora modelamos Palo y el Rol de Combate como strings, para mantenerlo simple
-- Sería mejor utilizar multiples constructores, con identificadores más apropiados, de la siguiente manera:
{-
data Palo = Corazon | Pica | Diamante | Trebol
  deriving (Eq, Show)

data RolCombate = Curador | Guerrero | Explosivo | Espia
  deriving (Eq, Show)
-}
-- De esta manera el mismo lenguaje detecta  errores tipográficos que con `String` pasarían inadvertidos. Por ejemplo, se coloca 
-- una carta con "espia" y otra con "espía" (con tilde), o "Diamante" y "diamante". 
-- Cuando la cantidad de elementos es finita y previsible, enumerar los posibles valores de un tipo de dato 
-- mediante múltiples constructores es la opción más segura.

-- --------------- Ejemplos

-- Ejemplos de cartas
carta1 :: CartaMagica
carta1 = UnaCartaMagica 5 "Corazon" True

carta2 :: CartaMagica
carta2 = UnaCartaMagica 3 "Pica" False

carta3 :: CartaMagica
carta3 = UnaCartaMagica 10 "Diamante" True

carta4 :: CartaMagica
carta4 = UnaCartaMagica 7 "Trebol" False

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

-- -----------------------------------------------------------------------------
-- --------------- PARTE II: PREPARANDO CAMPO DE BATALLA -----------------------

-- --------------- Energia Total

nombre :: SoldadoBlanco -> Nombre
nombre = fst

caracteristicas :: SoldadoBlanco -> [Caracteristica]
caracteristicas = snd

energiaBase :: Number
energiaBase = 50

energiaTotalReinaBlanca :: FuerzasBlancas -> Number
energiaTotalReinaBlanca (capitan, brujo) = poder brujo + sumaHabilidades capitan

sumaHabilidades :: SoldadoBlanco -> Number
sumaHabilidades (nom, []) = energia nom
sumaHabilidades (_, caract) = energiaBase + bonusPor "espada de petalo" caract + bonusCantHabilidades caract

energia Nombre -> Number
energia nombre = length nombre * 5

poder :: SoldadoBlanco -> Number
poder soldado 
  | null (habilidades soldado) = energia (nombre soldado)
  | otherwise = energiaBase + bonusPor "desaparacetus" (caracteristicas soldado)

bonusPor :: Caracteristica -> [Caracteristica] -> Number
bonusPor c cs
  | elem c cs = 10 
  | otherwise = 0
  

{-  
Si bien hay otras opciones que cumplen con el objetivo, cada una tiene ventajas y desventajas. 
La idea no es encontrar “la correcta”, sino entender qué decisiones de diseño hay detrás y qué impacto tienen.  

Todos los posibles integrantes de las Fuerzas Blancas son del mismo tipo: soldados, son su nombre y caracteristicas, 
que es un término genérico para referirse a armas, hechizos o lo que sea que caracterice al soldado.
No hay nada que los diferencie en su lógica, de todos puede calcularse su poder, habilidades, bonus, etc.
La diferencia está en lo que la Reina Blanca pide de ellos según el rol que ocupen en su ejercito, si de capitan o de mago:
al personaje que esté en ese momento como primer componente de la tupla (o sea capital) le pregunta el poder 
y al que está como segundo (mago) le pregunta su habilidad.
No es necesario validar que sea un Capitan o Brujo, porque los roles son intercambiables, son todos soldados. 

La lógica solicitada del cálculo de poder y de la habilidad son similares, por lo que para evitar repetir lógica se llama a funciones auxiliares.
Podría pensarse en alguna fórmula que generalice aún más, pero dada la particularidad del requerimiento quedaría muy compleja.
De esta manera, si bien persiste cierta similitud, queda facil de entender cómo se calcula cada cosa.
Para manejar la tupla del soldado, una de las soluciones usa pattern matching y la otra no sino que remite a funciones auxiliares
para cada componente de la tupla. Están a propósito así para que vean las variantes que existen, de manera similar a si hubieramos usado data.

No está bueno repetir código. Ahora puede parecer inofensivo, 
pero a medida que el sistema crece, mantenerlo se vuelve más difícil y propenso a errores.

Buscamos es que puedan ver las decisiones que tomamos como programadores. 
A veces tenemos que elegir entre flexibilidad y claridad, o entre reutilización y simplicidad. 
Lo importante es poder justificar esas elecciones y ser conscientes de sus consecuencias.

-}

bonusCantHabilidades :: [Caracteristica] -> Number
bonusCantHabilidades cs = length (take 3 cs) * 5 - length (drop 3 cs) * 3

{-
Otras formas posibles son: 

bonusCantHabilidades :: [Caracteristica] -> Number
bonusCantHabilidades cs
    | length cs <= 3 = length cs * 5
    | otherwise = 3 * 5 - (length cs - 3) * 3

Repite lógica y es díficil de mantener. Podemos ser más expresivos y aprovechar funciones como `min` y `max`
para lograr lo mismo con mayor claridad y estilo declarativo.

cantBaseHabilidades :: Number
cantBaseHabilidades = 3
  
bonusCantHabilidades :: [Caracteristica] -> Number
bonusCantHabilidades habilidades = bonusCantBase habilidades - multaSobrepeso habilidades

bonusCantBase :: [Habilidad] -> Number
bonusCantBase habilidades = min (length habilidades) cantBaseHabilidades * 5

multaSobrepeso :: [Habilidad] -> Number
multaSobrepeso habilidades = max (length habilidades - cantBaseHabilidades) 0 * 3 
-}

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
determinarRol :: CartaMagica -> RolCombate
determinarRol (UnaCartaMagica Corazon _ _) = Curador
determinarRol (UnaCartaMagica Pica _ _) = Guerrero
determinarRol (UnaCartaMagica Diamante nivel _) 
    | nivel > 5 = Explosivo
    | otherwise = Guerrero
determinarRol (UnaCartaMagica Trebol _ estado) 
    | estado = Espía 
    | otherwise = Guerrero

Otra opción para rol combate, usando pattern matching de DATA. 

-}

rolParaTrebol :: CartaMagica -> RolCombate
rolParaTrebol carta
  | estado carta = Espia
  | otherwise = Guerrero

-- Poder mágico individual de una carta
poderMagicoCarta :: CartaMagica -> Number
poderMagicoCarta carta
  | not(estado carta) = 0
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
-- poderMagicoReinaRoja (c1:c2:resto) = poderMagicoCarta c1 + poderMagicoCarta c2 + poderMagicoCarta (last resto)
-- Otra opcion posible. 

{-

Otra opcion usando guardas:

poderMagicoReinaRoja :: [CartaMagica] -> Number
poderMagicoReinaRoja cartas
  | null cartas = 0
  | length cartas == 1 = 3 * poderMagicoCarta carta
  | otherwise = poderMagicoCarta (head cartas) + poderMagicoCarta (cartas !! 1) + poderMagicoCarta (last cartas)

-}

{-
En cuanto a las **constantes**:

Muchas veces usar constantes con nombres claros ayuda a que el código sea más legible
y fácil de mantener. Por ejemplo, en lugar de poner un `3` suelto, poner `cantBaseHabilidades`
puede ayudar a entender de dónde viene ese número.

Sin embargo, ¡ojo! Usar demasiadas constantes, o constantes demasiado específicas,
puede hacer que el código se vuelva innecesariamente largo o difícil de leer.

La clave está en encontrar el equilibrio:
✔ Usar constantes que **agregan claridad**.
✖ Evitar las que **solo suman ruido**.
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
  | otherwise = cartaInfiltrada{estado = False}

esDivisible :: Number -> Number -> Bool
esDivisible dividendo divisor = mod dividendo divisor == 0

cambiarPalo :: CartaMagica -> Palo -> CartaMagica
cambiarPalo carta nuevoPalo = carta{palo = nuevoPalo}

cambiarNivel :: CartaMagica -> Number -> CartaMagica
cambiarNivel carta nuevoNivel = carta{nivel = nuevoNivel}

{-
Cuando transformamos una carta, vemos que se modifican varias propiedades (como el palo o el nivel).

Dado que estas modificaciones se repiten en distintas partes del código, es una muy buena idea
abstraer esa lógica en funciones auxiliares como `cambiarPalo` o `cambiarNivel`.

Esto no solo hace el código más limpio y reutilizable, también lo hace más declarativo y fácil de mantener.
-}

-- -----------------------------------------------------------------------------
-- --------------- PARTE III: ARCO DE ENTRENAMIENTO ---------------------------------

-- --------------- UTN: Universidad de Trucos y Nigromancia

-- Primera Funcion

aprenderHechizo :: SoldadoBlanco -> Habilidad -> SoldadoBlanco
aprenderHechizo soldado habilidad
  | puedeAprender soldado habilidad = (nombre soldado, habilidades soldado ++ [habilidad])
  | otherwise = soldado

puedeAprender :: SoldadoBlanco -> Habilidad  -> Bool
puedeAprender soldado habilidad = 
  not(habilidadYaDominada habilidad soldado) &&
  (primerasTresLetrasIguales (nombre soldado) habilidad ||
  length (habilidades soldado) > 100 )

primerasTresLetrasIguales :: String -> String -> Bool
primerasTresLetrasIguales p1 p2 = primerasTresLetras p1 == primerasTresLetras p2

primerasTresLetras :: String -> String
primerasTresLetras palabra = take 3 palabra

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
  | estado carta = cambiarNivel carta (nivel carta + 2)
  | otherwise = carta

transformar :: CartaMagica -> CartaMagica
transformar carta
  | poderMagicoCarta carta > 120 = cambiarPalo carta Diamante
  | poderMagicoCarta carta < 20  = cambiarPalo carta Corazon
  | otherwise = carta

-- --------------- Test de Equilibrio

formacionBalanceada :: [CartaMagica] -> Bool
formacionBalanceada ejercito = esPar ejercito && tieneRolesDiferentes ejercito

esPar :: [CartaMagica] -> Bool
esPar ejercito = even (length ejercito)

-- Recordamos que esto es un incorrecto uso de guardas:
{-
esCantidadDeCartasPar :: [Carta] -> Bool
esCantidadDeCartasPar cartas
    | even (length cartas) = True
    | otherwise= False
-}


tieneRolesDiferentes :: [CartaMagica] -> Bool
tieneRolesDiferentes ejercito = 
  rolEnCombate (head ejercito) /= rolEnCombate (last ejercito)

-- Escribir código expresivo no significa usar más palabras,
-- sino que el significado se entienda rápido y claro.
-- A veces, agregar nombres intermedios o constantes innecesarias
-- puede dificultar la lectura en vez de ayudar.

-- Por ejemplo: ultimaYPrimerCartaTienenRolesDiferentes, termina
-- siendo menos claro que tieneRolesDiferentes acompañado de su función. 
