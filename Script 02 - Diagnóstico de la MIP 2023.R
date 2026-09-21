# ==============================================================================
# SCRIPT 02 - DIAGNÓSTICO INICIAL DE LA MATRIZ INSUMO-PRODUCTO (BCE 2023)
# ==============================================================================

library(readxl)
library(dplyr)

mip <- read_xlsx("C:\\mip_2023.xlsm")


hojas_mip  <- excel_sheets("C:\\mip_2023.xlsm")

# ==============================================================================
#  CARGAR DE LA MATRIZ DE COEFICIENTES TÉCNICOS
# ==============================================================================


ruta_mip <- "C:\\mip_2023.xlsm"

# Cargamos la matriz de coeficientes técnicos
matriz_A_cruda <- read_xlsx(ruta_mip, sheet = "A(IxI)_bf")

# Verificamos sus dimensiones reales en la consola
print("Dimensiones de la pestaña de coeficientes técnicos:")
print(dim(matriz_A_cruda))

print(matriz_A_cruda[1:5, 1:5])

# ==============================================================================
# PASO 2: LIMPIEZA Y CONSTRUCCIÓN DE LA MATRIZ CUADRADA A (75 x 75)
# ==============================================================================

# Extraemos los nombres reales de las 75 industrias (están en la fila 3, desde la columna 3 hasta la 77)
nombres_industrias <- as.character(matriz_A_cruda[3, 3:77])
codigos_industrias <- as.character(matriz_A_cruda[2, 3:77]) # Ej: "01", "02", ...

# Filtramos las 75 filas de las industrias 
A_mat <- matriz_A_cruda[4:78, 3:77]

# Convertimos a matriz numérica pura en R para el álgebra lineal
A_mat <- matrix(as.numeric(as.matrix(A_mat)), nrow = 75, ncol = 75)

# Asignamos dimensiones para no perder el rastro de los sectores
rownames(A_mat) <- codigos_industrias
colnames(A_mat) <- codigos_industrias


# Busquemos los índices de las industrias clave
# ==============================================================================
# PASO 3: CONFIGURACIÓN DEL CHOQUE EXÓGENO (MODELO DE LEONTIEF SECTORIAL)
# ==============================================================================

# Definimos los índices sectoriales basados en tu diagnóstico
idx_combustibles <- which(colnames(A_mat) == "40") # Rama 40 (Refinados)
idx_otros_sectores <- which(colnames(A_mat) != "40") # Las otras 74 ramas

# Particionamos la matriz A original según el modelo de precios controlados
# Matriz transpuesta para el flujo de costos intersectoriales
A_transpuesta <- t(A_mat)

# Submatriz de coeficientes entre los sectores no regulados (74 x 74)
A_NC_NC <- A_transpuesta[idx_otros_sectores, idx_otros_sectores]

# Vector de coeficientes de los insumos que combustibles le vende a los demás sectores (74 x 1)
A_C_NC  <- A_transpuesta[idx_otros_sectores, idx_combustibles]

# Calculamos la matriz Identidad de tamaño 74 x 74 y la Inversa de Leontief parcial
I_NC <- diag(74)
Inversa_Leontief_Parcial <- solve(I_NC - A_NC_NC)

# Supongamos un escenario de choque inicial: incremento hipotético del 50% en combustibles
choque_combustibles <- 0.50 

# Calculamos la transmisión indirecta de precios
variacion_precios_indirecta <- Inversa_Leontief_Parcial %*% A_C_NC * choque_combustibles

df_impacto_macro <- data.frame(
  codigo = codigos_industrias[idx_otros_sectores],
  nombre = nombres_industrias[idx_otros_sectores],
  incremento_precio_porcentaje = as.numeric(variacion_precios_indirecta) * 100
) %>%
  arrange(desc(incremento_precio_porcentaje))

# 10 sectores más afectados por el alza de combustibles
print(head(df_impacto_macro, 10))

# ==============================================================================
# PASO 4: EXPORTACIÓN DE RESULTADOS MACRO PARA EL SCRIPT 03
# ==============================================================================

saveRDS(df_impacto_macro, "df_impacto_precios_leontief.rds")

# Diagnóstico 1: Ver las pestañas disponibles
print(hojas_mip)

# Diagnóstico 2: Ver los nombres reales de las industrias
print(nombres_industrias)

# Diagnóstico 3: Ver los códigos
print(codigos_industrias)

# Diagnóstico 4: ¿La matriz A tiene valores entre 0 y 1?
summary(as.vector(A_mat))

# Diagnóstico 5: ¿La suma de cada columna es menor a 1?
# (condición necesaria para que la inversa de Leontief exista)
colSumas <- colSums(A_mat, na.rm = TRUE)
summary(colSumas)
print(paste("Columnas con suma >= 1:", sum(colSumas >= 1)))

# Diagnóstico 6: ¿El sector 40 es realmente combustibles?
print(paste("Sector 40:", nombres_industrias[which(codigos_industrias == "40")]))

# Cargar la inversa de Leontief ya calculada por el BCE
inversa_leontief <- read_xlsx(ruta_mip, sheet = "(I-Aixi)-1_bf")

# Verificar dimensiones
print(dim(inversa_leontief))
print(inversa_leontief[1:3, 1:5])

# Ver las primeras 5 filas completas para entender la estructura
print(inversa_leontief[1:6, 1:6])

# Ver cuántas filas tienen datos reales
print(inversa_leontief[1:10, 1:3])

# Extraer la inversa de Leontief limpia
L_mat_cruda <- read_xlsx(ruta_mip, sheet = "(I-Aixi)-1_bf")

# Los datos reales empiezan en fila 4, columnas 3 a 77
L_mat <- L_mat_cruda[4:78, 3:77]

# Convertir a matriz numérica
L_mat <- matrix(as.numeric(as.matrix(L_mat)), nrow = 75, ncol = 75)

# Asignar nombres de sectores
rownames(L_mat) <- codigos_industrias
colnames(L_mat) <- codigos_industrias

# Validaciones
print("Dimensiones de la inversa de Leontief:")
print(dim(L_mat))

print("La diagonal debe ser mayor a 1 siempre:")
summary(diag(L_mat))

print("Sector 40 columna - multiplicadores hacia otros sectores:")
round(sort(L_mat[, "40"], decreasing = TRUE)[1:10], 4)


# ==============================================================================
# MODELO DE PRECIOS DE LEONTIEF - 4 ESCENARIOS
# ==============================================================================

# Índice del sector combustibles
idx_40 <- which(rownames(L_mat) == "40")

# Vector de coeficientes del sector 40 hacia todos los sectores
coef_combustibles <- L_mat[, idx_40]

# Definimos los 4 escenarios de choque en el precio de combustibles
escenarios <- list(
  E1_base        = 0,     # Sin reforma
  E2_eliminacion = 0.50,  # Eliminación total — ajustar con precio real
  E3_focalizacion = 0.25, # Focalización sectorial
  E4_optimo      = 0.30   # Reforma óptima con BDH
)

# Calculamos el vector de variación de precios para cada escenario
resultados_precios <- data.frame(
  codigo = codigos_industrias,
  nombre = nombres_industrias
)

for (nombre_esc in names(escenarios)) {
  choque <- escenarios[[nombre_esc]]
  delta_p <- coef_combustibles * choque
  resultados_precios[[nombre_esc]] <- delta_p
}

# Ver los 10 sectores más afectados en el escenario 2
print("Top 10 sectores más afectados — Escenario 2 (eliminación total):")
resultados_precios %>%
  arrange(desc(E2_eliminacion)) %>%
  select(codigo, nombre, E2_eliminacion) %>%
  head(10) %>%
  mutate(E2_eliminacion = round(E2_eliminacion * 100, 2)) %>%
  print()

# Guardar 
saveRDS(resultados_precios, "df_impacto_precios_leontief.rds")


#### este es el que estab bien 
# ==============================================================================
# SCRIPT 02 CORREGIDO - MODELO DE PRECIOS DE LEONTIEF - MIP 2023
# TESIS: Impacto fiscal y distributivo de la reforma a subsidios - ALEX PAUCAR
# ==============================================================================

library(readxl)
library(dplyr)

ruta_mip <- "C:\\mip_2023.xlsm"

# ==============================================================================
# PASO 1: CARGAR MATRIZ DE COEFICIENTES TÉCNICOS A
# ==============================================================================

matriz_A_cruda <- read_xlsx(ruta_mip, sheet = "A(IxI)_bf")

nombres_industrias <- as.character(matriz_A_cruda[3, 3:77])
codigos_industrias <- as.character(matriz_A_cruda[2, 3:77])

A_mat <- matriz_A_cruda[4:78, 3:77]
A_mat <- matrix(as.numeric(as.matrix(A_mat)), nrow = 75, ncol = 75)
rownames(A_mat) <- codigos_industrias
colnames(A_mat) <- codigos_industrias

# ==============================================================================
# PASO 2: DEFINICIÓN DE CHOQUES CON PRECIOS REALES
# ==============================================================================

precio_sub_extra   <- 3.310
precio_real_extra  <- 5.240
precio_sub_diesel  <- 3.250
precio_real_diesel <- 5.180

choque_extra  <- (precio_real_extra  - precio_sub_extra)  / precio_sub_extra
choque_diesel <- (precio_real_diesel - precio_sub_diesel) / precio_sub_diesel
choque_promedio <- 0.40 * choque_extra + 0.60 * choque_diesel

print(paste("Choque gasolina Extra:", round(choque_extra * 100, 1), "%"))
print(paste("Choque diésel:",         round(choque_diesel * 100, 1), "%"))
print(paste("Choque promedio ponderado:", round(choque_promedio * 100, 1), "%"))

escenarios <- list(
  E1_base         = 0,
  E2_eliminacion  = choque_promedio,
  E3_focalizacion = choque_promedio * 0.5,
  E4_optimo       = choque_promedio * 0.3
)

print("Choques por escenario:")
for (e in names(escenarios)) {
  print(paste(e, ":", round(escenarios[[e]] * 100, 1), "%"))
}

# ==============================================================================
# PASO 3: PARTICIÓN DE LA MATRIZ A PARA MODELO DE PRECIOS
# Formulación correcta lado costos:
# ΔP_NC = (I - A_NC_NC')⁻¹ × A_C_NC' × ΔP_C
# ==============================================================================

A_t    <- t(A_mat)
idx_40 <- which(colnames(A_mat) == "40")
idx_NC <- which(colnames(A_mat) != "40")

A_NC_NC <- A_t[idx_NC, idx_NC]
A_C_NC  <- A_t[idx_NC, idx_40]

I_NC <- diag(74)
Inversa_Precios_NC <- solve(I_NC - A_NC_NC)

print(paste("Dimensiones:", nrow(Inversa_Precios_NC), "x", ncol(Inversa_Precios_NC)))

# ==============================================================================
# PASO 4: SIMULACIÓN DE LOS 4 ESCENARIOS
# ==============================================================================
resultados_precios <- data.frame(
  codigo  = codigos_industrias,
  nombre  = nombres_industrias,
  E1_base = 0
)

for (nombre_esc in names(escenarios)) {
  if (nombre_esc == "E1_base") next
  
  choque <- escenarios[[nombre_esc]]
  
  # Variación en los 74 sectores no controlados
  delta_p_NC <- Inversa_Precios_NC %*% A_C_NC * choque
  
  # Vector completo de 75 sectores
  vector_p_total <- numeric(75)
  vector_p_total[idx_40] <- choque      # Sector 40 sube exactamente el choque
  vector_p_total[idx_NC] <- delta_p_NC  # Demás sectores suben por segunda ronda
  
  resultados_precios[[nombre_esc]] <- vector_p_total
}


# ==============================================================================
# PASO 5: RESULTADOS Y VALIDACIÓN
# ==============================================================================
resultados_precios %>%
  arrange(desc(E2_eliminacion)) %>%
  select(codigo, nombre, E2_eliminacion) %>%
  head(10) %>%
  mutate(E2_eliminacion = round(E2_eliminacion * 100, 2)) %>%
  print()

sectores_clave <- c("40", "60", "17", "18", "19", "22", "23", "59")
resultados_precios %>%
  filter(codigo %in% sectores_clave) %>%
  select(codigo, nombre, E1_base, E2_eliminacion, E3_focalizacion, E4_optimo) %>%
  mutate(across(c(E2_eliminacion, E3_focalizacion, E4_optimo),
                ~round(. * 100, 2))) %>%
  print()

# ==============================================================================
# PASO 6: EXPORTACIÓN 
# ==============================================================================
saveRDS(resultados_precios, "df_impacto_precios_leontief.rds")
saveRDS(escenarios,         "escenarios_choque.rds")
