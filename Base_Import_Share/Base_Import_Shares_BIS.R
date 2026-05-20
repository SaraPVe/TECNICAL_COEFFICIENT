required_packages <- c("writexl")
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = TRUE)
}

suppressWarnings(suppressPackageStartupMessages(library(writexl)))

###################
# VECTORES DE ORGANIZACIÓN DE DATOS
###################
load("Data/mis_sectores.RData")
load("Data/Data_origin_UNIZAR.RData")
data_BIS <- data_origin[1:2206, ]
data_BIS <- data_BIS[!(data_BIS[, 2] %in% c("TAXES_LESS_SUBSIDIES_ON_PRODUCTS", "VALUE_ADDED")), ]
data_BIS <- as.data.frame(data_BIS)

paises   <- data_BIS[, 1]
sectores <- data_BIS[, 2]

datos_numericos <- as.matrix(apply(data_BIS[, -c(1, 2)], 2, as.numeric))
datos_numericos[is.na(datos_numericos)] <- 0
columnas_totales <- colnames(data_BIS)[-c(1, 2)]

# Nombres de columnas (se reusan para numerador y denominador)
nombres_col <- c(
  "Pais", "Sector",
  paste(paises, sectores, sep = "_"),
  paste(rep(unique(paises), each = 6),
        rep(sectores_finales, times = length(unique(paises))), sep = "_")
)

###################
# FUNCIÓN AUXILIAR: calcula la matriz BIS (numerador o denominador)
#   excluir_fila: TRUE → numerador (excluye la fila actual), FALSE → denominador
###################
compute_BIS_matrix <- function(datos_num, paises, sectores, col_totales,
                               excluir_fila = FALSE) {
  resultado <- matrix(0, nrow = length(paises), ncol = ncol(datos_num))
  for (fila in seq_along(paises)) {
    cols_pais    <- which(startsWith(col_totales, paste0(paises[fila], "_")))
    filas_sector <- which(sectores == sectores[fila])
    if (excluir_fila) filas_sector <- setdiff(filas_sector, fila)

    if (length(cols_pais) > 0 && length(filas_sector) > 0) {
      resultado[fila, cols_pais] <- colSums(datos_num[filas_sector, cols_pais, drop = FALSE])
    }
  }
  df <- cbind(Pais = paises, Sector = sectores, as.data.frame(resultado))
  colnames(df) <- nombres_col
  df
}

###################
# NUMERADOR Y DENOMINADOR
###################
numerador_BIS_df    <- compute_BIS_matrix(datos_numericos, paises, sectores,
                                          columnas_totales, excluir_fila = TRUE)
denominador_BIS_df  <- compute_BIS_matrix(datos_numericos, paises, sectores,
                                          columnas_totales, excluir_fila = FALSE)

# Ordenación
orden_sectores <- match(sectores, sectores_prioritarios)
orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1
es_sector_final <- sectores %in% sectores_finales
orden_filas <- order(paises, es_sector_final, orden_sectores)

denominador_BIS_df <- denominador_BIS_df[orden_filas, ]
numerador_BIS_df   <- numerador_BIS_df[orden_filas, ]
paises   <- paises[orden_filas]
sectores <- sectores[orden_filas]

#####################  
# IMPORT SHARE FINAL
#####################
import_share <- numerador_BIS_df[, -c(1, 2)] / denominador_BIS_df[, -c(1, 2)]
import_share[is.na(import_share)] <- 0
import_share <- cbind(denominador_BIS_df[, c(1, 2)], import_share)
rownames(import_share) <- paste(paises, sectores, sep = "_")
import_share <- as.data.frame(import_share)

#####################
# TRASPOSICIÓN: extraer submatriz por país y apilar
#####################
paises_filas    <- sub("_.*", "",   rownames(import_share))
paises_columnas <- sub("_.*", "",   colnames(import_share))

lista_matrices <- lapply(unique(paises_columnas), function(pais) {
  filas_pais    <- which(paises_filas    == pais)
  columnas_pais <- which(paises_columnas == pais)
  if (length(filas_pais) == 0 || length(columnas_pais) == 0) return(NULL)
  subm <- import_share[filas_pais, columnas_pais, drop = FALSE]
  colnames(subm) <- sub("^[^_]*_", "", colnames(subm))
  subm
})

resultado_df <- as.data.frame(do.call(rbind, Filter(Negate(is.null), lista_matrices)))

if (ncol(resultado_df) == length(sectores_columna)) {
  colnames(resultado_df) <- sectores_columna
}

# Diagnóstico: celdas fuera de rango [0, 1]
n_mayores1  <- sum(resultado_df > 1,  na.rm = TRUE)
n_menores0  <- sum(resultado_df < 0,  na.rm = TRUE)
if (n_mayores1 > 0 || n_menores0 > 0) {
  message("Aviso: ", n_mayores1, " celda(s) > 1 y ",
          n_menores0, " celda(s) < 0 encontradas. Se recortan a [0, 1].")
}

# Recorte: forzar import share a [0, 1]
resultado_df <- as.data.frame(lapply(resultado_df, function(x) pmin(pmax(x, 0), 1)))

write_xlsx(resultado_df, "./Base_Import_Share/Base_Import_Share_R.xlsx")
