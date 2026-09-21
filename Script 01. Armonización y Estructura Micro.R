# ==============================================================================
# TESIS DE MAESTRÍA (MPP) - ALEX PAUCAR
# Tema: Impacto fiscal y distributivo de la reforma a subsidios en Ecuador
# SCRIPT 01: Armonización de Datos y Estructura de Gasto Micro
# Fuente: ENIGHUR 2025
# ==============================================================================

# PASO 1: LIBRERÍAS
library(dplyr)
library(readr)
library(tidyr)
library(survey)
library(ineq)
library(openxlsx)
library(readxl)


# ==============================================================================
# PASO 2: RUTAS DE DATOS
# ==============================================================================
ruta_gastos  <- "C:\\Nueva ENIGHUR\\Bases de trabajo_csv"


# ==============================================================================
# PASO 3: CARGA DE MICRODATOS
# ==============================================================================

df_gastos  <- ENIGHUR2025_GASTOS_HMO
df_hogares <- ENIGHUR2025_HOGARES_AGREGADOS

# ==============================================================================
# PASO 4: EXTRACCIÓN DE GASTO EN COMBUSTIBLES POR HOGAR
# Columnas validadas directamente en la consola:
# c11111101 = Gasolina Extra/Ecopaís (2,305 hogares)
# c11111102 = Gasolina Súper         (1,277 hogares)
# c11111103 = Diésel vehicular       (9,031 hogares)
# c1114001  = GLP cilindro doméstico (4,141 hogares)
# c1114002  = GLP tanque doméstico   (955 hogares)
# ==============================================================================

gasto_combustibles_hogar <- df_gastos %>%
  mutate(
    Identif_hog         = as.character(Identif_hog),
    gasto_extra_ecopais = as.numeric(c11111101),
    gasto_super         = as.numeric(c11111102),
    gasto_diesel        = as.numeric(c11111103),
    gasto_glp_hogar     = as.numeric(c1114001) + as.numeric(c1114002),
    gasto_anual_combustibles = rowSums(
      cbind(gasto_extra_ecopais, gasto_super, gasto_diesel, gasto_glp_hogar),
      na.rm = TRUE)
  ) %>%
  mutate(across(c(gasto_extra_ecopais, gasto_super, gasto_diesel,
                  gasto_glp_hogar, gasto_anual_combustibles),
                ~replace_na(., 0))) %>%
  select(Identif_hog, gasto_extra_ecopais, gasto_super,
         gasto_diesel, gasto_glp_hogar, gasto_anual_combustibles)


print(paste("Hogares con gasto en gasolina extra:", 
            sum(gasto_combustibles_hogar$gasto_extra_ecopais > 0)))
print(paste("Hogares con gasto en diésel:", 
            sum(gasto_combustibles_hogar$gasto_diesel > 0)))
print(paste("Hogares con gasto en GLP:", 
            sum(gasto_combustibles_hogar$gasto_glp_hogar > 0)))

# ==============================================================================
# PASO 5: MERGE ENTRE HOGARES Y COMBUSTIBLES
# ==============================================================================

df_hogares <- df_hogares %>%
  mutate(Identif_hog = as.character(Identif_hog))

base_micro_unificada <- df_hogares %>%
  left_join(gasto_combustibles_hogar, by = "Identif_hog") %>%
  mutate(across(c(gasto_extra_ecopais, gasto_super, gasto_diesel,
                  gasto_glp_hogar, gasto_anual_combustibles),
                ~replace_na(., 0)))

print(paste("Hogares en base unificada:", nrow(base_micro_unificada)))

# Verificación del merge
print(paste("Hogares con combustibles > 0 tras merge:", 
            sum(base_micro_unificada$gasto_anual_combustibles > 0)))

# ==============================================================================
# PASO 6: PREPARACIÓN DE VARIABLES SOCIOECONÓMICAS
# ==============================================================================

base_micro_unificada <- base_micro_unificada %>%
  mutate(
    ingreso_analisis = as.numeric(ing_cor_tot),
    gasto_t_analisis = as.numeric(gas_cor_tot),
    factor_analisis  = as.numeric(Fexp)
  ) %>%
  filter(!is.na(factor_analisis) & factor_analisis > 0)
summary(base_micro_unificada$ingreso_analisis)

# ==============================================================================
# PASO 7: DISEÑO MUESTRAL Y QUINTILES PONDERADOS
# ==============================================================================

# Solución para estratos con una sola UPM
options(survey.lonely.psu = "adjust")

diseño_enighur <- svydesign(
  id      = ~Identif_upm,
  strata  = ~estrato,
  weights = ~factor_analisis,
  data    = base_micro_unificada,
  nest    = TRUE
)

cortes_quintiles <- svyquantile(~ingreso_analisis, diseño_enighur, 
                                quantiles = c(0.2, 0.4, 0.6, 0.8))
valores_corte <- as.numeric(cortes_quintiles$ingreso_analisis)

print(paste("Q1-Q2 (P20):", round(valores_corte[1], 2)))
print(paste("Q2-Q3 (P40):", round(valores_corte[2], 2)))
print(paste("Q3-Q4 (P60):", round(valores_corte[3], 2)))
print(paste("Q4-Q5 (P80):", round(valores_corte[4], 2)))

base_micro_unificada <- base_micro_unificada %>%
  mutate(quintil = case_when(
    ingreso_analisis <= valores_corte[1] ~ 1,
    ingreso_analisis <= valores_corte[2] ~ 2,
    ingreso_analisis <= valores_corte[3] ~ 3,
    ingreso_analisis <= valores_corte[4] ~ 4,
    TRUE ~ 5
  ))

print("Distribución de hogares por quintil:")
print(table(base_micro_unificada$quintil))

# ==============================================================================
# PASO 8: CÁLCULO DEL OMEGA (participación del gasto en combustibles)
# ==============================================================================
base_micro_final <- base_micro_unificada %>%
  mutate(
    omega_ih = if_else(gasto_t_analisis > 0, 
                       gasto_anual_combustibles / gasto_t_analisis, 
                       0))


summary(base_micro_final$omega_ih)

base_micro_final %>%
  group_by(quintil) %>%
  summarise(
    omega_promedio    = mean(omega_ih, na.rm = TRUE),
    hogares           = n()
  ) %>%
  print()

# ==============================================================================
# PASO 9: EXPORTACIÓN
# ==============================================================================

if(!dir.exists("datos_procesados")) dir.create("datos_procesados")

saveRDS(base_micro_final, "datos_procesados/base_enighur_2025_armonizada.rds")

# Validación 1: ¿Cuántos hogares tienen gasto en combustibles?
sum(base_micro_final$gasto_extra_ecopais > 0, na.rm = TRUE)
sum(base_micro_final$gasto_diesel > 0, na.rm = TRUE)
sum(base_micro_final$gasto_glp_hogar > 0, na.rm = TRUE)

# Validación 2: ¿Cómo se distribuyen los hogares por quintil?
table(base_micro_final$quintil)

# Validación 3: ¿El Omega tiene sentido? (debe estar entre 0 y 1)
summary(base_micro_final$omega_ih)


base_micro_final %>%
  group_by(quintil) %>%
  summarise(
    omega_promedio = round(mean(omega_ih, na.rm = TRUE), 4),
    gasto_combustible_promedio = round(mean(gasto_anual_combustibles, na.rm = TRUE), 2),
    hogares = n())