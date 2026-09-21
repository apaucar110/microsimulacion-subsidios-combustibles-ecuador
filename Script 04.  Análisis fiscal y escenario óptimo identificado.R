# ==============================================================================
# SCRIPT 04 - AHORRO FISCAL NETO Y ESCENARIO ÓPTIMO
# TESIS: Impacto fiscal y distributivo de la reforma a subsidios - ALEX PAUCAR
# ==============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)
library(dplyr)

# ==============================================================================
# PASO 1: CARGAR RESULTADOS PREVIOS
# ==============================================================================

base_micro <- readRDS("datos_procesados/base_microsimulacion.rds")
resultados_fgt <- readRDS("datos_procesados/resultados_fgt_gini.rds")

# ==============================================================================
# PASO 2: SUBSIDIOS REALES 2024
# Fuente: EP Petroecuador - SPF (marzo 2026)
# ==============================================================================
subsidios_2024 <- read.xlsx("C:\\TESIS MAESTRIA\\subsidios_derivados_de_petroleo marzo 2026.xlsx",
                            sheet = 1, startRow = 6)

combustible_selec <- c("GLP","DIESEL", "GASOLINAS", "FUEL OIL",
                 "RESIDUOS", "JET FUEL", "AZUFRE", "ASFALTO","AVGAS")

subsidios_2024 <- subsidios_2024 %>%
  select("COMBUSTIBLE","2024") %>%
  filter(COMBUSTIBLE %in% combustible_selec) %>%
  rename(sub_2024 = `2024`)


subsidio_total_2024 <- sum(subsidios_2024$sub_2024)

print("Subsidios por combustible 2024 (millones USD):")
print(subsidios_2024)
print(paste("TOTAL subsidios 2024:", round(subsidio_total_2024, 2), "millones USD"))

# Subsidios relevantes para los escenarios (GLP + Diesel + Gasolinas)
subsidio_relevante <- subsidios_2024 %>%
  filter(COMBUSTIBLE %in% c("DIESEL", "GASOLINAS")) %>%
  summarise(total = sum(sub_2024)) %>%
  pull(total)


print(paste("Subsidio relevante (DIESEL+GASOLINAS):", 
            round(subsidio_relevante, 2), "millones USD"))


# ==============================================================================
# PASO 3: AHORRO FISCAL BRUTO POR ESCENARIO
# ==============================================================================
ahorro_fiscal <- data.frame(
  escenario    = c("E1_base", "E2_eliminacion", 
                   "E3_focalizacion", "E4_optimo"),
  descripcion  = c(
    "Sin reforma",
    "Eliminación total sin compensación",
    "Focalización sectorial (50% del choque)",
    "Reforma óptima con BDH (30% del choque)"),
  porcentaje_eliminacion = c(0, 1.00, 0.50, 0.30),
  ahorro_bruto_mill = c(0,
                        subsidio_relevante * 1.00,
                        subsidio_relevante * 0.50,
                        subsidio_relevante * 0.30))

print("Ahorro fiscal bruto por escenario:")
print(ahorro_fiscal[, c("escenario", "descripcion", "ahorro_bruto_mill")])

# ==============================================================================
# PASO 4: COSTO DE COMPENSACIÓN BDH
# Calcular cuánto costaría compensar a los quintiles 1 y 2
# ==============================================================================
print("Calculando costo de compensación BDH...")

# Población de hogares expandida por factor de expansión
hogares_por_quintil <- base_micro %>%
  group_by(quintil) %>%
  summarise(
    hogares_expandidos = sum(factor_exp, na.rm = TRUE),
    CC_E2_promedio     = mean(CC_E2, na.rm = TRUE),
    CC_E3_promedio     = mean(CC_E3, na.rm = TRUE),
    CC_E4_promedio     = mean(CC_E4, na.rm = TRUE),
    .groups = "drop")

# Costo de compensar completamente a Q1 y Q2 en cada escenario
costo_bdh <- hogares_por_quintil %>%
  filter(quintil %in% c(1, 2)) %>%
  summarise(
    costo_E2_mill = sum(hogares_expandidos * CC_E2_promedio) * 12 / 1e6,
    costo_E3_mill = sum(hogares_expandidos * CC_E3_promedio) * 12 / 1e6,
    costo_E4_mill = sum(hogares_expandidos * CC_E4_promedio) * 12 / 1e6 )

print(round(costo_bdh, 2))


# ==============================================================================
# PASO 5: AHORRO FISCAL NETO
# Ahorro Neto = Ahorro Bruto - Costo Compensación
# ==============================================================================

resultados_fiscales <- data.frame(
  escenario   = c("E1", "E2", "E3", "E4"),
  descripcion = c(
    "Sin reforma",
    "Eliminación total",
    "Focalización sectorial",
    "Reforma óptima con BDH"
  ),
  ahorro_bruto_mill = c(
    0,
    ahorro_fiscal$ahorro_bruto_mill[2],
    ahorro_fiscal$ahorro_bruto_mill[3],
    ahorro_fiscal$ahorro_bruto_mill[4]
  ),
  costo_bdh_mill = c(
    0,
    costo_bdh$costo_E2_mill,
    costo_bdh$costo_E3_mill,
    costo_bdh$costo_E4_mill
  )
) %>%
  mutate(
    ahorro_neto_mill = ahorro_bruto_mill - costo_bdh_mill,
    eficiencia_pct   = ifelse(ahorro_bruto_mill > 0,
                              ahorro_neto_mill / ahorro_bruto_mill * 100, 0)
  )

print(resultados_fiscales %>%
        mutate(across(where(is.numeric), ~round(., 2))))


# ==============================================================================
# PASO 6: TABLA RESUMEN FINAL — RESPUESTA A LA PREGUNTA DE INVESTIGACIÓN
# ==============================================================================
tabla_final <- resultados_fiscales %>%
  left_join(resultados_fgt, by = "escenario") %>%
  select(
    escenario, descripcion,
    ahorro_bruto_mill, costo_bdh_mill, ahorro_neto_mill,
    P0, Gini
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(tabla_final)

# Identificar escenario óptimo
escenario_optimo <- tabla_final %>%
  filter(escenario != "E1") %>%
  arrange(desc(ahorro_neto_mill), P0) %>%
  slice(1)

print(escenario_optimo)
print(paste("El escenario", escenario_optimo$escenario, 
            "-", escenario_optimo$descripcion,
            "genera el mayor ahorro fiscal neto de",
            round(escenario_optimo$ahorro_neto_mill, 2),
            "millones USD con una tasa de pobreza de",
            escenario_optimo$P0, "%"))


# ==============================================================================
# PASO 7: EXPORTAR RESULTADOS
# ==============================================================================
saveRDS(resultados_fiscales, "datos_procesados/resultados_fiscales.rds")
saveRDS(tabla_final,         "datos_procesados/tabla_final_escenarios.rds")

write.csv(tabla_final, "datos_procesados/tabla_final_escenarios.csv", 
          row.names = FALSE)

