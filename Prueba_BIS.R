############################################################
# Paquetes
############################################################

# (Solo hace falta instalarlos una vez)
# install.packages(c("readxl", "tidyr", "dplyr", "openxlsx", "readr", "writexl"))

library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(writexl)

############################################################
# VECTORES DE ORGANIZACIÓN DE DATOS
############################################################

sectores_columna <- c(
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
  "PUBLIC_ADMINISTRATION", "EDUCATION", "HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS",
  "HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE", "NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS",
  "GENERAL_GOVERNMENT_FINAL_CONSUMPTION", "GROSS_FIXED_CAPITAL_FORMATION",
  "CHANGE_IN_INVENTORIES_AND_VALUABLES", "DIRECT_PURCHASES_ABROAD"
)

sectores_prioritarios <- c(
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

sectores_finales <- c(
  "HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE",
  "NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS",
  "GENERAL_GOVERNMENT_FINAL_CONSUMPTION",
  "GROSS_FIXED_CAPITAL_FORMATION",
  "CHANGE_IN_INVENTORIES_AND_VALUABLES",
  "DIRECT_PURCHASES_ABROAD"
)

Country <- c(
  "AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECHREPUBLIC","DENMARK",
  "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
  "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS",
  "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN",
  "UK", "CHINA", "EASOC", "INDIA", "LATAM", "RUSSIA", "USMCA", "LROW"
)

############################################################
# NUMERADOR: IMPORTACIONES POR PAÍS DESTINO Y SECTOR
############################################################

# Leer excel
data_BIS_origin <- read.xlsx("./Base_Import_Share/DATA_BIS_ORIGIN.xlsx", colNames = TRUE)
data_BIS_origin <- data_BIS_origin[1:2206, , drop = FALSE]

# Quitamos TAXES y VALUE_ADDED
data_BIS <- data_BIS_origin[!(data_BIS_origin[,2] %in%
                                c("TAXES_LESS_SUBSIDIES_ON_PRODUCTS","VALUE_ADDED")),
                            , drop = FALSE]
data_BIS <- as.data.frame(data_BIS)

# Países y sectores (filas)
paises   <- data_BIS[,1]
sectores <- data_BIS[,2]

# Parte numérica
numeric_cols    <- 3:ncol(data_BIS)
datos_numericos <- apply(data_BIS[, numeric_cols, drop = FALSE], 2, as.numeric)
datos_numericos[is.na(datos_numericos)] <- 0
datos_numericos <- as.matrix(datos_numericos)

columnas_totales <- colnames(data_BIS)[numeric_cols]

# Matriz numerador
numerador_BIS <- matrix(0, nrow = nrow(data_BIS), ncol = length(numeric_cols))

for (fila in seq_len(nrow(data_BIS))) {
  pais_actual   <- paises[fila]
  sector_actual <- sectores[fila]
  
  # Columnas del mismo país destino
  columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
  
  # Filas del mismo sector (todos los países) excepto la fila actual
  filas_mismo_sector <- which(sectores == sector_actual)
  filas_a_sumar      <- setdiff(filas_mismo_sector, fila)
  
  if (length(columnas_mismo_pais) > 0 && length(filas_a_sumar) > 0) {
    suma_columnas <- colSums(datos_numericos[filas_a_sumar,
                                             columnas_mismo_pais,
                                             drop = FALSE])
    numerador_BIS[fila, columnas_mismo_pais] <- suma_columnas
  }
}

numerador_BIS_df <- cbind(
  Pais   = paises,
  Sector = sectores,
  as.data.frame(numerador_BIS)
)
colnames(numerador_BIS_df) <- colnames(data_BIS)

############################################################
# DENOMINADOR: TOTAL (DOMÉSTICO + IMPORTADO) POR PAÍS DESTINO Y SECTOR
############################################################

denominador_BIS <- matrix(0, nrow = nrow(data_BIS), ncol = length(numeric_cols))

for (fila in seq_len(nrow(data_BIS))) {
  pais_actual   <- paises[fila]
  sector_actual <- sectores[fila]
  
  columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
  filas_mismo_sector  <- which(sectores == sector_actual)
  
  if (length(columnas_mismo_pais) > 0 && length(filas_mismo_sector) > 0) {
    suma_columnas <- colSums(datos_numericos[filas_mismo_sector,
                                             columnas_mismo_pais,
                                             drop = FALSE])
    denominador_BIS[fila, columnas_mismo_pais] <- suma_columnas
  }
}

denominador_BIS_df <- cbind(
  Pais   = paises,
  Sector = sectores,
  as.data.frame(denominador_BIS)
)
colnames(denominador_BIS_df) <- colnames(data_BIS)

############################################################
# ORDENACIÓN DE FILAS (PAÍS, TIPO DE SECTOR, PRIORIDAD)
############################################################

orden_sectores <- match(sectores, sectores_prioritarios)
orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1

es_sector_final <- sectores %in% sectores_finales

orden_paises <- match(paises, Country)
orden_paises[is.na(orden_paises)] <- length(Country) + 1

orden_filas <- order(orden_paises, es_sector_final, orden_sectores)

numerador_BIS_df   <- numerador_BIS_df[orden_filas, , drop = FALSE]
denominador_BIS_df <- denominador_BIS_df[orden_filas, , drop = FALSE]

paises_ord   <- paises[orden_filas]
sectores_ord <- sectores[orden_filas]

############################################################
# IMPORT SHARE FINAL (INTERMEDIATE_IMPORT_SHARE_RAW)
############################################################

intermediate_import_share_raw <- numerador_BIS_df

intermediate_import_share_raw[, numeric_cols] <-
  numerador_BIS_df[, numeric_cols] /
  denominador_BIS_df[, numeric_cols]

# Sustituir NA/Inf por 0
mat_tmp <- as.matrix(intermediate_import_share_raw[, numeric_cols])
mat_tmp[!is.finite(mat_tmp)] <- 0
intermediate_import_share_raw[, numeric_cols] <- mat_tmp

# Rownames País_Sector ya ordenados
rownames(intermediate_import_share_raw) <- paste(paises_ord, sectores_ord, sep = "_")

############################################################
# (Opcional) pivot_longer para inspección
############################################################

a <- intermediate_import_share_raw %>% 
  pivot_longer(
    cols = -c(Pais, Sector),
    names_to = c("Country2", "Sector2"),
    names_pattern = "([^_]*)_(.*)"
  )

############################################################
# TRASPOSICIÓN / AGRUPACIÓN POR PAÍS DESTINO
############################################################

intermediate_import_share_raw <- as.data.frame(intermediate_import_share_raw)

# Nombres de filas y columnas numéricas
paises_sectores_filas    <- rownames(intermediate_import_share_raw)
paises_sectores_columnas <- colnames(intermediate_import_share_raw)[numeric_cols]

# País y sector de filas (Pais_Sector)
paises_filas   <- sub("_.*",      "",  paises_sectores_filas)
sectores_filas <- sub("^[^_]*_",  "",  paises_sectores_filas)

# País y sector de columnas (Pais_Sector_destino)
paises_columnas   <- sub("_.*",      "",  paises_sectores_columnas)
sectores_columnas <- sub("^[^_]*_",  "",  paises_sectores_columnas)

# Lista para almacenar submatrices por país destino
lista_matrices <- list()

for (pais in unique(paises_columnas)) {
  filas_pais    <- which(paises_filas    == pais)
  columnas_pais <- which(paises_columnas == pais)
  
  if (length(filas_pais) == 0 || length(columnas_pais) == 0) next
  
  submatriz <- intermediate_import_share_raw[filas_pais,
                                             numeric_cols[columnas_pais],
                                             drop = FALSE]
  lista_matrices[[pais]] <- submatriz
}

# Quitar prefijo de país en los nombres de columnas (de "FRANCE_CROPS" a "CROPS")
lista_matrices2 <- lapply(lista_matrices, function(df) {
  colnames(df) <- sub("^[^_]*_", "", colnames(df))
  df
})

# Alinear todas las submatrices al conjunto de sectores_columna
all_cols   <- unique(unlist(lapply(lista_matrices2, colnames)))
col_order  <- intersect(sectores_columna, all_cols)

lista_matrices_alineadas <- lapply(lista_matrices2, function(df) {
  missing_cols <- setdiff(col_order, colnames(df))
  if (length(missing_cols) > 0) {
    for (mc in missing_cols) df[[mc]] <- 0
  }
  df <- df[, col_order, drop = FALSE]
  df
})

# Apilar todo
resultado_matriz <- do.call(rbind, lista_matrices_alineadas)
resultado_df     <- as.data.frame(resultado_matriz)

# Añadir País y Sector a partir de los rownames
row_ids     <- rownames(resultado_df)
Pais_out    <- sub("_.*",     "",  row_ids)
Sector_out  <- sub("^[^_]*_", "",  row_ids)

resultado_df <- cbind(Pais = Pais_out,
                      Sector = Sector_out,
                      resultado_df)

############################################################
# EXPORTAR A EXCEL
############################################################

write_xlsx(resultado_df, "./Base_Import_Share/Base_Import_Share_R.xlsx")
