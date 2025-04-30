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

-- Aunque es posible usar `String` en lugar de constructores para modelar el palo y el rol de combate, 
-- preferimos usar múltiples constructores. Esto se debe a que al trabajar con `String` podrían surgir 
-- errores tipográficos. Por ejemplo, se coloca una carta con "espia" y otra con "espía" (con tilde), o "Diamante" y "diamante". Además, 
-- usar múltiples constructores ayuda a evitar errores comunes, como escribir "basto" en una de las cartas (cuando aqui no es una opcion).
-- Aunque el uso de `String` no está prohibido, la opción de usar constructores múltiples es más segura y menos propensa a fallos.

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

-- Es importante notar que "DaliaCentenaria" y "GatoDeCheshire" están modelados como ejemplos de "SoldadoBlanco",
-- lo que permite una mayor flexibilidad. Si bien en este caso forman parte de las "Fuerzas Blancas", 
-- podrían cambiar o ampliarse en el futuro sin necesidad de modificar la estructura del código.

-- Evita modelar "DaliaCentenaria" y "GatoDeCheshire" como tipos independientes (como se muestra en el ejemplo
-- comentado más abajo), ya que esto dificultaría los cambios y la ampliación del sistema en el futuro.
{-
data DaliaCentenaria = UnSoldadoBlanco {
  nombre :: Nombre,
  habilidades :: [armas]
}
-}

-- Tampoco es recomendable definir un ejército blanco como un tipo rígido con los soldados ya definidos,
-- ya que esto también limita la posibilidad de hacer cambios en el futuro. 
-- En su lugar, usamos la definición flexible de "FuerzasBlancas", que permite agregar o cambiar los soldados 
-- sin afectar la estructura general.
{-
type EjercitoBlanco = (daliaCentenaria, gatoDeCheshire)
-}

-- Usando este enfoque flexible, podemos agregar nuevos soldados o modificar los existentes sin necesidad de 
-- modificar la estructura central del código, lo cual facilita la evolución del programa.

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
Podemos aprovechar una característica del lenguaje que nos permite simplificar definiciones,
como si estuviéramos resolviendo una ecuación matemática.

Al igual que en las ecuaciones, solo podemos "eliminar" elementos que estén sueltos y aparezcan
en ambos lados de la definición. Esto aplica, por ejemplo, a parámetros pero NO a funciones.

Por ejemplo:

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad personaje = calculoComunPersonajes personaje

Se puede simplificar a:

sumaHabilidad :: (String, [String]) -> Number
sumaHabilidad = calculoComunPersonajes

Esta forma es más concisa y expresa lo mismo. 
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
De la misma forma habiamos calculado el peso de un pino en el TP anterior. 

Recordamos que algo así:

energiaPorCantArmas :: [String] -> Number
energiaPorCantArmas armas
    | length armas <= 3 = length armas * 5
    | otherwise = 3 * 5 - (length armas - 3) * 3

No está mal, pero en este caso podemos ser más expresivos y aprovechar funciones como `min` y `max`
para lograr lo mismo con mayor claridad y estilo declarativo.
-}

bonusCantBase :: [Habilidad] -> Number
bonusCantBase habilidades = min (length habilidades) cantBaseHabilidades * puntosBase

multaSobrepeso :: [Habilidad] -> Number
multaSobrepeso habilidades = max (length habilidades - cantBaseHabilidades) 0 * cantMulta 

bonusElemento :: String -> [Habilidad] -> Number
bonusElemento eltoClave habilidades
  | elem eltoClave habilidades = 10
  | otherwise = 0

{-
Veamos un ejemplo importante sobre **reutilización de código**:

Supongamos que teníamos dos funciones como estas:

tieneEspadaDePetalo :: DaliaCentenaria -> Bool
tieneEspadaDePetalo dalia = "Espada de petalo" `elem` armas dalia

tieneDesaparecetus :: GatoDeCheshire -> Bool
tieneDesaparecetus gato = "Desaparecetus" `elem` hechizos gato

Aunque ambas funciones son correctas y "declaran bien" lo que hacen,
repiten una lógica muy similar: verificar si un string está presente
en una lista de strings.

Cuando vean que el enunciado les pide algo muy parecido en distintos lugares,
es una excelente oportunidad para **abstraer** esa lógica y reutilizarla.

En esta materia valoramos mucho que puedan detectar estos patrones y los generalicen.

Recordá: cada vez que copiás y pegás código, un gatito muere :)
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
Sobre condicionales:

Sabemos que en otros cursos (¡y en internet!) pueden encontrar el uso de estructuras
como `if-then-else`, `where`, `let`, `case`, `maybe`, entre otras. 

Sin embargo, en esta cursada NO las vamos a usar, ni las vamos a evaluar.

Cuando necesiten representar condiciones, utilicen **guardas** como lo venimos haciendo.

Esto no significa que estén mal, solo que queremos enfocarnos en un subconjunto del lenguaje
para aprender programación funcional con una base sólida y consistente.
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

Otra opción para rol combate.

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
-- poderMagicoReinaRoja (c1:c2:resto) = poderMagicoCarta c1 + poderMagicoCarta c2 + poderMagicoCarta (last resto)
-- Otra opcion posible. 

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
  | otherwise = cartaInfiltrada{estado = Inactiva}

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

{-
Por otro lado, una tentación puede ser generalizar de más:

-- Esto NO se puede hacer:
cambiarPropiedad carta propiedad valor = carta{propiedad = valor}

Haskell no permite modificar dinámicamente cualquier campo de un registro
solo con el nombre de la propiedad. Así que, en este caso, abstraer por separado
cada cambio es lo más adecuado.
-}


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

-- Escribir código expresivo no significa usar más palabras,
-- sino que el significado se entienda rápido y claro.
-- A veces, agregar nombres intermedios o constantes innecesarias
-- puede dificultar la lectura en vez de ayudar.

-- Por ejemplo: 
{-
verificarNuevoPoderMagicoYAsignarPalo :: Number -> CartaMagica -> CartaMagica

intercambiarRolesDeEjercitoReinaBlanca :: EjercitoReinaBlanca -> EjercitoReinaBlanca

nroPrimerasLetrasDeNombreCoincidentes :: Number
-}
