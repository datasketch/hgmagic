# Ejemplos de uso de la función data_processing
# =============================================

# Cargar librerías necesarias
library(hgmagic)
library(dsdatawiz)
library(dplyr)
library(lubridate)

# =============================================
# Ejemplo 1: Datos con categorías únicamente
# =============================================

# Crear datos de ejemplo con solo categorías
data_categorias <- data.frame(
  categoria = c("A", "B", "C", "A", "B", "C", "A", "B"),
  subcategoria = c("X", "Y", "Z", "X", "Y", "Z", "X", "Y"),
  stringsAsFactors = FALSE
)

# Procesar datos con solo categorías
resultado_categorias <- data_processing(
  data = data_categorias,
  var_group = c("categoria", "subcategoria")
)

print("Ejemplo 1 - Datos con categorías únicamente:")
print(resultado_categorias)

# =============================================
# Ejemplo 2: Datos con categorías y variables numéricas
# =============================================

# Crear datos de ejemplo con categorías y valores numéricos
data_cat_num <- data.frame(
  region = c("Norte", "Sur", "Este", "Oeste", "Norte", "Sur", "Este", "Oeste"),
  producto = c("A", "B", "A", "B", "A", "B", "A", "B"),
  ventas = c(100, 150, 200, 120, 180, 160, 220, 140),
  ganancia = c(20, 30, 40, 25, 35, 32, 44, 28),
  stringsAsFactors = FALSE
)

# Procesar datos con categorías y variables numéricas
resultado_cat_num <- data_processing(
  data = data_cat_num,
  var_group = c("region", "producto"),
  var_num = c("ventas", "ganancia"),
  tooltip_template = "Región: {region} <br/> Producto: {producto} <br/> Suma Ventas: {ventas} <br/> Suma Ganancias: {ganancia}"
)

print("\nEjemplo 2 - Datos con categorías y variables numéricas:")
print(resultado_cat_num)

# =============================================
# Ejemplo 3: Datos sin variables categóricas (solo numéricas)
# =============================================

# Crear datos de ejemplo con solo variables numéricas
data_numericas <- data.frame(
  valor1 = c(10, 20, 30, 40, 50),
  valor2 = c(15, 25, 35, 45, 55),
  valor3 = c(12, 22, 32, 42, 52),
  stringsAsFactors = FALSE
)

# Procesar datos sin variables categóricas
resultado_numericas <- data_processing(
  data = data_numericas,
  var_group = NULL,
  var_num = c("valor1", "valor2")
)

print("\nEjemplo 3 - Datos sin variables categóricas:")
print(resultado_numericas)

# =============================================
# Ejemplo 4: Datos con categorías y fechas
# =============================================

# Crear datos de ejemplo con categorías y fechas
data_cat_fecha <- data.frame(
  categoria = c("A", "B", "A", "B", "A", "B"),
  fecha = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02",
                   "2023-01-02", "2023-01-03", "2023-01-03")),
  valor = c(100, 150, 200, 120, 180, 160),
  stringsAsFactors = FALSE
)

# Procesar datos con categorías y fechas
resultado_cat_fecha <- data_processing(
  data = data_cat_fecha,
  var_group = c("categoria", "fecha"),
  var_num = "valor"
)

print("\nEjemplo 4 - Datos con categorías y fechas:")
print(resultado_cat_fecha)

# =============================================
# Ejemplo 5: Datos con categorías, fechas y variables numéricas
# =============================================

# Crear datos de ejemplo más complejos
data_completo <- data.frame(
  region = c("Norte", "Sur", "Este", "Oeste", "Norte", "Sur", "Este", "Oeste"),
  fecha = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01",
                    "2023-01-02", "2023-01-02", "2023-01-02", "2023-01-02")),
  ventas = c(100, 150, 200, 120, 180, 160, 220, 140),
  costos = c(80, 120, 160, 100, 140, 130, 180, 110),
  stringsAsFactors = FALSE
)

# Procesar datos completos
resultado_completo <- data_processing(
  data = data_completo,
  var_group = c("region", "fecha"),
  var_num = c("ventas", "costos")
)

print("\nEjemplo 5 - Datos con categorías, fechas y variables numéricas:")
print(resultado_completo)

# =============================================
# Ejemplo 6: Uso con diccionario de datos
# =============================================

# Crear un diccionario de datos de ejemplo
dic_ejemplo <- data.frame(
  id = c("region", "fecha", "ventas", "costos"),
  label = c("region", "fecha", "ventas", "costos"),
  hdt = c("Cat", "Dat", "Num", "Num"),
  stringsAsFactors = FALSE
)

# Procesar datos usando el diccionario
resultado_con_dic <- data_processing(
  data = data_completo,
  dic = dic_ejemplo,
  var_group = "region",
  var_num = c("ventas")
)

print("\nEjemplo 6 - Uso con diccionario de datos:")
print(resultado_con_dic)

# =============================================
# Ejemplo 7: Datos con años en lugar de fechas
# =============================================

# Crear datos de ejemplo con años
data_con_anos <- data.frame(
  categoria = c("A", "B", "A", "B", "A", "B"),
  año = c(2020, 2020, 2021, 2021, 2022, 2022),
  valor = c(100, 150, 200, 120, 180, 160),
  stringsAsFactors = FALSE
)

# Procesar datos con años
resultado_con_anos <- data_processing(
  data = data_con_anos,
  var_group = c("categoria", "año"),
  var_num = "valor"
)

print("\nEjemplo 7 - Datos con categorías y años:")
print(resultado_con_anos)

# =============================================
# Ejemplo 8: Datos con múltiples niveles de categorías
# =============================================

# Crear datos de ejemplo con múltiples niveles
data_multinivel <- data.frame(
  continente = c("América", "Europa", "Asia", "América", "Europa", "Asia"),
  pais = c("México", "España", "Japón", "Brasil", "Francia", "China"),
  ciudad = c("CDMX", "Madrid", "Tokio", "São Paulo", "París", "Beijing"),
  poblacion = c(9000000, 3200000, 14000000, 12000000, 2100000, 22000000),
  stringsAsFactors = FALSE
)

# Procesar datos multinivel
resultado_multinivel <- data_processing(
  data = data_multinivel,
  var_group = c("continente", "pais", "ciudad"),
  var_num = "poblacion"
)

print("\nEjemplo 8 - Datos con múltiples niveles de categorías:")
print(resultado_multinivel)

