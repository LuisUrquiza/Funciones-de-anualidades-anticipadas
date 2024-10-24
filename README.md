# Funciones de Anualidades Anticipadas
En este repositorio tengo un archivo README que contiene funciones para cálculo de anualidades anticipadas en R

# Función para calcular el valor futuro de una anualidad anticipada
valor_futuro_anualidad = function(anualidad, tasa_periodo, plazo) {
 valor_futuro = anualidad * ((1 + tasa_periodo) ^ plazo - 1) / tasa_periodo
 return(valor_futuro)
}

# Función para calcular la anualidad de una anualidad anticipada
anualidad_valor_futuro = function(valor_futuro, tasa_periodo, plazo) {
 anualidad = valor_futuro * tasa_periodo / ((1 + tasa_periodo) ^ plazo - 1)
 return(anualidad)
}

# Función para calcular el número de pagos o plazo de una anualidad anticipada
plazo_anualidad = function(valor_futuro, anualidad, tasa_periodo) {
 plazo = log((valor_futuro * tasa_periodo / anualidad) + 1) / log(1 + tasa_periodo)
 return(plazo)
}

# Función para calcular la tasa del período de una anualidad anticipada
tasa_periodo_anualidad = function(valor_futuro, anualidad, plazo) {
 tasa_periodo = (anualidad / valor_futuro) * ((1 + tasa_periodo) ^ plazo - 1) / plazo
 # Debido a que la ecuación no se puede resolver algebraicamente de forma sencilla,
 # se puede utilizar un método numérico como el siguiente:
 tasa_periodo = uniroot(function(x) (anualidad / valor_futuro) * ((1 + x) ^ plazo - 1) / plazo - x, c(0, 1))$root
 return(tasa_periodo)
}

# Función para calcular el valor actual de una anualidad anticipada
valor_actual_anualidad = function(anualidad, tasa_periodo, plazo) {
 valor_actual = anualidad * (1 - (1 + tasa_periodo) ^ (-plazo)) / tasa_periodo
 return(valor_actual)
}

# Función para calcular la anualidad de una anualidad anticipada a partir del valor actual
anualidad_valor_actual = function(valor_actual, tasa_periodo, plazo) {
 anualidad = valor_actual * tasa_periodo / (1 - (1 + tasa_periodo) ^ (-plazo))
 return(anualidad)
}

# Función para calcular el número de pagos o plazo de una anualidad anticipada a partir del valor actual
plazo_valor_actual = function(valor_actual, anualidad, tasa_periodo) {
 plazo = log(1 - (anualidad / (valor_actual * tasa_periodo))) / log(1 + tasa_periodo)
 return(plazo)
}

# Función para calcular la tasa del período de una anualidad anticipada a partir del valor actual
tasa_periodo_valor_actual = function(valor_actual, anualidad, plazo) {
 # Debido a que la ecuación no se puede resolver algebraicamente de forma sencilla,
 # se puede utilizar un método numérico como el siguiente:
 tasa_periodo = uniroot(function(x) anualidad / (valor_actual * (1 - (1 + x) ^ (-plazo) / x)) - 1, c(0, 1))$root
 return(tasa_periodo)
}
