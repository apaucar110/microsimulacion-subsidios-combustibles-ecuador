# ==============================================================================
# SCRIPT 03 DEFINITIVO
# ==============================================================================

library(haven)
library(dplyr)
library(survey)
library(tidyr)

# PASO 1: CARGAR BASES
base_enighur <- readRDS("datos_procesados/base_enighur_2025_armonizada.rds")
df_precios   <- readRDS("df_impacto_precios_leontief.rds")
escenarios   <- readRDS("escenarios_choque.rds")

ruta_personas <- "C:\\TESIS MAESTRIA\\ENEMDU-2024 anual spss\\BDDenemdu_personas_2024_anual.sav"
df_personas <- read_sav(ruta_personas) %>% rename_with(tolower)
print(paste("Personas ENEMDU:", nrow(df_personas)))

# PASO 2: BASE DE HOGARES ENEMDU
df_hogares_enemdu <- df_personas %>%
  filter(as.numeric(p04) == 1) %>%
  mutate(
    ingreso_pc      = as.numeric(ingpc),
    ingreso_laboral = as.numeric(ingrl),
    factor_exp      = as.numeric(fexp),
    area_hogar      = as.numeric(area),
    prov_hogar      = as.numeric(prov),
    pobreza_base    = as.numeric(pobreza),
    epobreza_base   = as.numeric(epobreza)
  ) %>%
  filter(!is.na(factor_exp) & factor_exp > 0) %>%
  mutate(
    ingreso_pc = ifelse(is.na(ingreso_pc),
                        median(ingreso_pc, na.rm = TRUE),
                        ingreso_pc)
  ) %>%
  select(id_hogar, id_vivienda, upm, estrato, fexp,
         factor_exp, ingreso_pc, ingreso_laboral,
         area_hogar, prov_hogar, condact,
         pobreza_base, epobreza_base)

print(paste("Hogares construidos:", nrow(df_hogares_enemdu)))

# PASO 3: QUINTILES PONDERADOS
options(survey.lonely.psu = "adjust")
diseño_enemdu <- svydesign(
  id = ~upm, strata = ~estrato,
  weights = ~factor_exp,
  data = df_hogares_enemdu, nest = TRUE
)
cortes_q  <- svyquantile(~ingreso_pc, diseño_enemdu,
                         quantiles = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
valores_q <- as.numeric(cortes_q$ingreso_pc)

df_hogares_enemdu <- df_hogares_enemdu %>%
  mutate(quintil = case_when(
    ingreso_pc <= valores_q[1] ~ 1,
    ingreso_pc <= valores_q[2] ~ 2,
    ingreso_pc <= valores_q[3] ~ 3,
    ingreso_pc <= valores_q[4] ~ 4,
    TRUE ~ 5
  ))

print("Quintiles ENEMDU:")
print(table(df_hogares_enemdu$quintil))

# PASO 4: CALCULAR CC CON VECTOR COMPLETO DE PRECIOS
crosswalk <- list(
  d1  = c("17","18","19","22","23","28","29","30"),
  d2  = c("32","33"),
  d3  = c("34","35","36"),
  d4  = c("40","56","57","58"),
  d5  = c("54","55"),
  d6  = c("43","73","74"),
  d7  = c("60","61","53"),
  d8  = c("64"),
  d9  = c("63","75"),
  d10 = c("71","72"),
  d11 = c("63"),
  d12 = c("65","66","67"),
  d13 = c("59","69","70")
)

divisiones <- names(crosswalk)

delta_coicop <- data.frame(division = divisiones)
for (esc in c("E2_eliminacion","E3_focalizacion","E4_optimo")) {
  delta_coicop[[esc]] <- sapply(crosswalk, function(sectores) {
    mean(df_precios[[esc]][df_precios$codigo %in% sectores], na.rm = TRUE)
  })
}

print("Delta P por división COICOP — Escenario 2 (%):")
print(round(delta_coicop$E2_eliminacion * 100, 2))

# Calcular omega por división y CC en ENIGHUR
base_enighur_cc <- base_enighur %>%
  mutate(
    gasto_total = as.numeric(gas_cor_tot),
    across(all_of(divisiones), as.numeric)
  ) %>%
  filter(!is.na(gasto_total) & gasto_total > 0)

for (div in divisiones) {
  base_enighur_cc[[paste0("omega_", div)]] <-
    base_enighur_cc[[div]] / base_enighur_cc$gasto_total
}

for (esc in c("E2_eliminacion","E3_focalizacion","E4_optimo")) {
  cc_hogar <- rep(0, nrow(base_enighur_cc))
  for (i in seq_along(divisiones)) {
    div      <- divisiones[i]
    omega_div <- base_enighur_cc[[paste0("omega_", div)]]
    delta_div <- delta_coicop[[esc]][i]
    cc_hogar  <- cc_hogar + replace_na(omega_div, 0) * delta_div
  }
  base_enighur_cc[[paste0("CC_", esc)]] <- cc_hogar
}

# Omega promedio por quintil y área desde ENIGHUR
omega_quintil <- base_enighur_cc %>%
  group_by(quintil, AREA) %>%
  summarise(
    CC_E2_prom = mean(CC_E2_eliminacion,  na.rm = TRUE),
    CC_E3_prom = mean(CC_E3_focalizacion, na.rm = TRUE),
    CC_E4_prom = mean(CC_E4_optimo,       na.rm = TRUE),
    gasto_prom = mean(gasto_total,        na.rm = TRUE),
    .groups = "drop"
  )

print(omega_quintil)

# PASO 5: UNIR CON ENEMDU Y CALCULAR CC EN DÓLARES
base_micro_simulacion <- df_hogares_enemdu %>%
  mutate(AREA = area_hogar) %>%
  left_join(omega_quintil, by = c("quintil", "AREA")) %>%
  mutate(
    CC_E1 = 0,
    CC_E2 = replace_na(CC_E2_prom, 0) * ingreso_pc,
    CC_E3 = replace_na(CC_E3_prom, 0) * ingreso_pc,
    CC_E4 = replace_na(CC_E4_prom, 0) * ingreso_pc
  )

base_micro_simulacion %>%
  group_by(quintil) %>%
  summarise(
    CC_E2_usd = round(mean(CC_E2, na.rm = TRUE), 2),
    CC_E4_usd = round(mean(CC_E4, na.rm = TRUE), 2),
    hogares   = n()
  ) %>%
  print()

# PASO 6: FGT Y GINI

linea_pobreza <- 90.29

calcular_fgt <- function(ingreso, linea, pesos) {
  pobres <- ingreso < linea
  P0 <- weighted.mean(pobres, pesos, na.rm = TRUE)
  brecha <- ifelse(pobres, (linea - ingreso) / linea, 0)
  P1 <- weighted.mean(brecha, pesos, na.rm = TRUE)
  P2 <- weighted.mean(brecha^2, pesos, na.rm = TRUE)
  return(c(P0 = P0, P1 = P1, P2 = P2))
}

calcular_gini <- function(ingreso, pesos) {
  ord     <- order(ingreso)
  ing_ord <- ingreso[ord]
  pes_ord <- pesos[ord]
  pes_cum <- cumsum(pes_ord) / sum(pes_ord)
  ing_cum <- cumsum(ing_ord * pes_ord) / sum(ing_ord * pes_ord)
  gini    <- 1 - 2 * sum(diff(pes_cum) *
                           (ing_cum[-length(ing_cum)] + ing_cum[-1]) / 2)
  return(gini)
}

resultados_fgt <- data.frame()

for (esc in c("E1", "E2", "E3", "E4")) {
  cc_col  <- paste0("CC_", esc)
  ing_adj <- base_micro_simulacion$ingreso_pc -
    base_micro_simulacion[[cc_col]]
  ing_adj <- pmax(ing_adj, 0)
  
  fgt  <- calcular_fgt(ing_adj, linea_pobreza,
                       base_micro_simulacion$factor_exp)
  gini <- calcular_gini(ing_adj, base_micro_simulacion$factor_exp)
  
  resultados_fgt <- rbind(resultados_fgt, data.frame(
    escenario = esc,
    P0   = round(fgt["P0"] * 100, 2),
    P1   = round(fgt["P1"] * 100, 2),
    P2   = round(fgt["P2"] * 100, 2),
    Gini = round(gini, 4)
  ))
}

print(resultados_fgt)

# PASO 7: EXPORTAR
saveRDS(base_micro_simulacion, "datos_procesados/base_microsimulacion.rds")
saveRDS(resultados_fgt,        "datos_procesados/resultados_fgt_gini.rds")
