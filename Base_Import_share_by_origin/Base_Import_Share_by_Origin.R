###################
# LIBRERIAS
###################
required_packages <- c("dplyr", "tidyr", "writexl")
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = TRUE)
}

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(writexl)
}))
###################
# CARGA DE DATOS Y VECTORES
###################
load("Data/Data_origin_UNIZAR.RData")    # data_origin

# 62 sectores validos
sector_orden_original <- c(
  "CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL", "EXTRACTION_OIL",
  "EXTRACTION_GAS", "EXTRACTION_OTHER_GAS", "MINING_AND_MANUFACTURING_URANIUM_THORIUM",
  "MINING_AND_MANUFACTURING_IRON", "MINING_AND_MANUFACTURING_COPPER",
  "MINING_AND_MANUFACTURING_NICKEL", "MINING_AND_MANUFACTURING_ALUMINIUM",
  "MINING_AND_MANUFACTURING_PRECIOUS_METALS", "MINING_AND_MANUFACTURING_LEAD_ZINC_TIN",
  "MINING_AND_MANUFACTURING_OTHER_METALS", "MINING_NON_METALS", "MANUFACTURE_FOOD",
  "MANUFACTURE_WOOD", "COKE", "REFINING", "MANUFACTURE_CHEMICAL", "MANUFACTURE_PLASTIC",
  "MANUFACTURE_OTHER_NON_METAL", "HYDROGEN_PRODUCTION", "MANUFACTURE_METAL_PRODUCTS",
  "MANUFACTURE_ELECTRONICS", "MANUFACTURE_ELECTRICAL_EQUIPMENT", "MANUFACTURE_MACHINERY",
  "MANUFACTURE_VEHICLES", "MANUFACTURE_OTHER", "ELECTRICITY_COAL", "ELECTRICITY_GAS",
  "ELECTRICITY_NUCLEAR", "ELECTRICITY_HYDRO", "ELECTRICITY_WIND", "ELECTRICITY_OIL",
  "ELECTRICITY_SOLAR_PV", "ELECTRICITY_SOLAR_THERMAL", "ELECTRICITY_OTHER",
  "DISTRIBUTION_ELECTRICITY", "DISTRIBUTION_GAS", "STEAM_HOT_WATER", "WASTE_MANAGEMENT",
  "CONSTRUCTION", "TRADE_REPAIR_VEHICLES", "TRANSPORT_RAIL", "TRANSPORT_OTHER_LAND",
  "TRANSPORT_PIPELINE", "TRANSPORT_SEA", "TRANSPORT_INLAND_WATER", "TRANSPORT_AIR",
  "ACCOMMODATION", "TELECOMMUNICATIONS", "FINANCE", "REAL_ESTATE", "OTHER_SERVICES",
  "PUBLIC_ADMINISTRATION", "EDUCATION", "HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS"
)

data_BIS <- data_origin[1:2206, ]
data_BIS <- data_BIS[data_BIS$Sector %in% sector_orden_original, ]
###################
# 1) PASO A FORMATO LARGO (BASE)
###################
Numerador_BISO_raw <- data_BIS %>%
  pivot_longer(
    cols = -c(Pais, Sector),
    names_to = "Pais_columna",
    values_to = "Valor"
  ) %>%
load("Data/Data_origin_UNIZAR.RData")    # data_origin

# 62 sectores validos
sector_orden_original <- c(
  "CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL", "EXTRACTION_OIL",
  "EXTRACTION_GAS", "EXTRACTION_OTHER_GAS", "MINING_AND_MANUFACTURING_URANIUM_THORIUM",
  "MINING_AND_MANUFACTURING_IRON", "MINING_AND_MANUFACTURING_COPPER",
  "MINING_AND_MANUFACTURING_NICKEL", "MINING_AND_MANUFACTURING_ALUMINIUM",
  "MINING_AND_MANUFACTURING_PRECIOUS_METALS", "MINING_AND_MANUFACTURING_LEAD_ZINC_TIN",
  "MINING_AND_MANUFACTURING_OTHER_METALS", "MINING_NON_METALS", "MANUFACTURE_FOOD",
  "MANUFACTURE_WOOD", "COKE", "REFINING", "MANUFACTURE_CHEMICAL", "MANUFACTURE_PLASTIC",
  "MANUFACTURE_OTHER_NON_METAL", "HYDROGEN_PRODUCTION", "MANUFACTURE_METAL_PRODUCTS",
  "MANUFACTURE_ELECTRONICS", "MANUFACTURE_ELECTRICAL_EQUIPMENT", "MANUFACTURE_MACHINERY",
  "MANUFACTURE_VEHICLES", "MANUFACTURE_OTHER", "ELECTRICITY_COAL", "ELECTRICITY_GAS",
  "ELECTRICITY_NUCLEAR", "ELECTRICITY_HYDRO", "ELECTRICITY_WIND", "ELECTRICITY_OIL",
  "ELECTRICITY_SOLAR_PV", "ELECTRICITY_SOLAR_THERMAL", "ELECTRICITY_OTHER",
  "DISTRIBUTION_ELECTRICITY", "DISTRIBUTION_GAS", "STEAM_HOT_WATER", "WASTE_MANAGEMENT",
  "CONSTRUCTION", "TRADE_REPAIR_VEHICLES", "TRANSPORT_RAIL", "TRANSPORT_OTHER_LAND",
  "TRANSPORT_PIPELINE", "TRANSPORT_SEA", "TRANSPORT_INLAND_WATER", "TRANSPORT_AIR",
  "ACCOMMODATION", "TELECOMMUNICATIONS", "FINANCE", "REAL_ESTATE", "OTHER_SERVICES",
  "PUBLIC_ADMINISTRATION", "EDUCATION", "HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS"
)

data_BIS <- data_origin[1:2206, ]
data_BIS <- data_BIS[data_BIS$Sector %in% sector_orden_original, ]
###################
# 1) PASO A FORMATO LARGO (BASE)
###################
Numerador_BISO_raw <- data_BIS %>%
  pivot_longer(
    cols = -c(Pais, Sector),
    names_to = "Pais_columna",
    values_to = "Valor"
  ) %>%
  separate(
    col = Pais_columna,
    into = c("Pais_col", "Sector_col"),
    sep = "_",
    extra = "merge"
  ) %>%
  mutate(
    across(c(Pais, Sector, Pais_col), ~ trimws(gsub("\\s+", " ", as.character(.)))),
    Sector_limpio = sub("\\d+$", "", Sector_col)
  ) %>%
  select(-Sector_col) %>%
  filter(Sector %in% sector_orden_original,
         Sector_limpio %in% sector_orden_original)

###################
# 2) CALCULO TIPO EXCEL (SI.ERROR(...,0) y diagonal a 0)
###################
BISO_long <- Numerador_BISO_raw %>%
  group_by(Pais, Sector, Pais_col, Sector_limpio) %>%
  summarise(
    numerador = sum(Valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Pais_col, Sector, Sector_limpio) %>%
  mutate(
    denominador = sum(numerador[Pais != Pais_col], na.rm = TRUE),
    share = case_when(
      Pais == Pais_col        ~ 0,
      denominador == 0        ~ 0,
      is.na(denominador)      ~ 0,
      TRUE                    ~ numerador / denominador
    )
  ) %>%
  ungroup()

###################
# 3) VOLVER A ANCHO (FORMATO FINAL)
#    + ORDENAR COLUMNAS-SECTOR EN EL MISMO ORDEN QUE LOS SECTORES EN FILAS
###################

BISO <- BISO_long %>%
  select(Pais, Sector_Fila = Sector, Pais_col, Sector_limpio, share) %>%
  pivot_wider(names_from = Sector_limpio, values_from = share)

cols_id <- c("Pais_col", "Sector_Fila", "Pais")
cols_sector_actuales <- setdiff(names(BISO), cols_id)

# Interseccion manteniendo el orden de las filas; extras al final
cols_sector_ordenadas <- intersect(sector_orden_original, cols_sector_actuales)
cols_sector_extras    <- setdiff(cols_sector_actuales, cols_sector_ordenadas)
cols_sector_final     <- c(cols_sector_ordenadas, sort(cols_sector_extras))
BISO <- BISO %>%
  select(all_of(cols_id), all_of(cols_sector_final))

###################
# 4) ORDEN, FACTORES Y NA a 0
###################
BISO <- BISO %>%
  mutate(
    Sector_Fila = factor(as.character(Sector_Fila), levels = sector_orden_original),
    across(where(is.numeric), ~ replace_na(., 0))
  ) %>%
  arrange(Pais, Sector_Fila, Pais_col)

# Comprobacion
any(is.na(BISO))

###################
# 6) DIAGNÓSTICO Y RECORTE A [0, 1]
###################
cols_numericas <- names(BISO)[sapply(BISO, is.numeric)]

n_mayores1 <- sum(BISO[, cols_numericas] > 1, na.rm = TRUE)
n_menores0 <- sum(BISO[, cols_numericas] < 0, na.rm = TRUE)
if (n_mayores1 > 0 || n_menores0 > 0) {
  message("Aviso: ", n_mayores1, " celda(s) > 1 y ",
          n_menores0, " celda(s) < 0 encontradas. Se recortan a [0, 1].")
}

BISO[, cols_numericas] <- lapply(BISO[, cols_numericas],
                                  function(x) pmin(pmax(x, 0), 1))

###################
# 7) EXPORTAR
###################
write_xlsx(BISO,"./Base_Import_share_by_origin/BISO.xlsx")