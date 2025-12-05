#install.packages(c("readxl", "tidyr", "dplyr", "openxlsx", "readr"))
#install.packages("writexl")

# Lectura del excel
library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(writexl)

###################
# VECTORES DE ORGANIZACIÓN DE DATOS
###################
load("data/mis_sectores.RData")
###################
# NUMERADOR 
###################

data_BIS_origin<-read.xlsx("DATA_BIS_ORIGIN.xlsx", colNames = TRUE)
data_BIS_origin<-data_BIS_origin[1:2206,]
data_BIS<-data_BIS_origin[!(data_BIS_origin[,2] %in% c("TAXES_LESS_SUBSIDIES_ON_PRODUCTS","VALUE_ADDED")),]
any(is.na(data_BIS))
data_BIS <- as.data.frame(data_BIS)

paises   <- data_BIS[,1]
sectores <- data_BIS[,2]

datos_numericos <- as.matrix(apply(data_BIS[, -c(1, 2)], 2, as.numeric))
datos_numericos[is.na(datos_numericos)] <- 0

numerador_BIS   <- matrix(0, nrow = nrow(data_BIS), ncol = ncol(data_BIS) - 2)
columnas_totales <- colnames(data_BIS)[-c(1,2)]

for (fila in 1:nrow(data_BIS)) {
  pais_actual   <- paises[fila]
  sector_actual <- sectores[fila]
  
  columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
  filas_mismo_sector  <- which(sectores == sector_actual)
  filas_a_sumar       <- setdiff(filas_mismo_sector, fila)

  if (length(columnas_mismo_pais) > 0 && length(filas_a_sumar) > 0) {
    suma_columnas <- colSums(as.matrix(datos_numericos[filas_a_sumar, columnas_mismo_pais, drop = FALSE]))
    numerador_BIS[fila, columnas_mismo_pais] <- suma_columnas
  }
}

numerador_BIS_df <- as.data.frame(numerador_BIS)
numerador_BIS_df <- cbind(Pais = paises, Sector = sectores, numerador_BIS_df)

nombres_columnas_regulares <- paste(paises, sectores, sep = "_")
paises_repetidos <- rep(unique(paises), each = 6)
nombres_columnas_finales <- paste(paises_repetidos,
                                  rep(sectores_finales, times = length(unique(paises))), sep = "_")

colnames(numerador_BIS_df) <- c("Pais", "Sector" ,nombres_columnas_regulares, nombres_columnas_finales)

###################
# DENOMINADOR
###################

denominador_BIS <- matrix(0, nrow = nrow(data_BIS), ncol = ncol(data_BIS) - 2)
columnas_totales <- colnames(data_BIS)[-c(1,2)]

for (fila in 1:nrow(data_BIS)) {
  pais_actual   <- paises[fila]
  sector_actual <- sectores[fila]
  
  datos_numericos <- as.matrix(apply(data_BIS[, -c(1, 2)], 2, as.numeric))
  columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
  filas_mismo_sector  <- which(sectores == sector_actual)
  
  if (length(columnas_mismo_pais) > 0 && length(filas_mismo_sector) > 0) {
    suma_columnas <- colSums(as.matrix(datos_numericos[filas_mismo_sector,
                                                       columnas_mismo_pais, drop = FALSE]))
    denominador_BIS[fila, columnas_mismo_pais] <- suma_columnas
  }
}

denominador_BIS_df <- as.data.frame(denominador_BIS)
denominador_BIS_df <- cbind(Pais = paises, Sector = sectores, denominador_BIS_df)

nombres_columnas_regulares <- paste(paises, sectores, sep = "_")
paises_repetidos <- rep(unique(paises), each = 6)
nombres_columnas_finales <- paste(paises_repetidos,
                                  rep(sectores_finales, times = length(unique(paises))), sep = "_")

colnames(denominador_BIS_df) <- c("Pais", "Sector" ,nombres_columnas_regulares, nombres_columnas_finales)

# Ordenación
orden_sectores <- match(sectores, sectores_prioritarios)
orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1
es_sector_final <- sectores %in% sectores_finales
orden_paises <- match(paises, Country)
orden_paises[is.na(orden_paises)] <- length(Country) + 1

orden_filas <- order(paises, es_sector_final, orden_sectores)

# APLICAR MISMO ORDEN A TODO  ### CAMBIO
denominador_BIS_df <- denominador_BIS_df[orden_filas, ]
numerador_BIS_df   <- numerador_BIS_df[orden_filas, ]
paises   <- paises[orden_filas]
sectores <- sectores[orden_filas]

#####################  
# IMPORT SHARE FINAL
#####################

### CAMBIO: quitar el lío de %>% en la división
intermediate_import_share_raw <- numerador_BIS_df[,-c(1,2)] / denominador_BIS_df[,-c(1,2)]

intermediate_import_share_raw[is.na(intermediate_import_share_raw)]<- 0

### CAMBIO: usar base R en vez de %>%
intermediate_import_share_raw <- cbind(denominador_BIS_df[,c(1,2)], intermediate_import_share_raw)

rownames(intermediate_import_share_raw) <- paste(paises, sectores, sep = "_")

#####################
# TRASPOSICIÓN
#################### 

intermediate_import_share_raw <- as.data.frame(intermediate_import_share_raw)

### CAMBIO GORDO AQUÍ: pivot_longer estaba roto (cols = -c(-1,-2), coma final y %>% colgando)
a <- intermediate_import_share_raw %>% 
  pivot_longer(
    cols = -c(Pais, Sector),
    names_to = c("Country2", "Sector2"),
    names_pattern = "([^_]*)_(.*)"
  )

# Obtener los nombres originales de las filas y columnas
paises_sectores_filas    <- rownames(intermediate_import_share_raw)
paises_sectores_columnas <- colnames(intermediate_import_share_raw)

# Extraer país y sector de las filas usando los rownames  ### CAMBIO
paises_filas   <- sub("_.*","",  paises_sectores_filas)
sectores_filas <- sub("^[^_]*_","",paises_sectores_filas)

# Extraer país y sector de las columnas
paises_columnas   <- sub("_.*","",  paises_sectores_columnas)
sectores_columnas <- sub("^[^_]*_","",paises_sectores_columnas)

# Lista para almacenar las submatrices por país
lista_matrices <- list()

for (pais in unique(paises_columnas)) {
  filas_pais    <- which(paises_filas    == pais)
  columnas_pais <- which(paises_columnas == pais)
  
  if (length(filas_pais) == 0 || length(columnas_pais) == 0) next
  
  ### CAMBIO: drop = FALSE para no perder dimensión
  submatriz <- intermediate_import_share_raw[filas_pais, columnas_pais, drop = FALSE]
  lista_matrices[[pais]] <- submatriz
}

lista_matrices2 <- lapply(lista_matrices, function(df) {
  colnames(df) <- sub("^[^_]*_", "", colnames(df))
  return(df)
})

resultado_matriz <- do.call(rbind, lista_matrices2)
resultado_df     <- as.data.frame(resultado_matriz)

### CAMBIO: solo renombrar si coincide nº de columnas, para evitar error
if (ncol(resultado_df) == length(sectores_columna)) {
  colnames(resultado_df) <- sectores_columna
}

# (opcional) podrías añadir País y Sector aquí si quieres
# row_ids     <- rownames(resultado_df)
# Pais_out    <- sub("_.*","",   row_ids)
# Sector_out  <- sub("^[^_]*_","",row_ids)
# resultado_df <- cbind(Pais = Pais_out, Sector = Sector_out, resultado_df)
resultado_df[resultado_df==0]<-0.00001
write_xlsx(resultado_df, "./Base_Import_Share/Base_Import_Share_R.xlsx")
any(is.na(resultado_df))
rm(list = ls())
