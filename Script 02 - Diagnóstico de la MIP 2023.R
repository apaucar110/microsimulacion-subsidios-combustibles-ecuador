
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
