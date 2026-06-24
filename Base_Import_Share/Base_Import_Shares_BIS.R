library(tidyverse)
library(openxlsx)

###################
# RUTAS Y PARAMETROS
###################

obtener_directorio_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }

  cwd <- normalizePath(getwd())
  if (basename(cwd) == "Base_Import_Share") {
    return(cwd)
  }
  normalizePath(file.path(cwd, "Base_Import_Share"))
}

localizar_wiliam <- function(project_root) {
  candidatos <- c(
    file.path(dirname(project_root), "WILIAM"),
    project_root
  )
  valido <- candidatos[
    file.exists(file.path(
      candidatos,
      "model_parameters/economy/Trade.xlsx"
    ))
  ]

  if (length(valido) == 0) {
    stop("No se encontro el repositorio WILIAM con Trade.xlsx.")
  }
  normalizePath(valido[[1]])
}

script_dir <- obtener_directorio_script()
project_root <- normalizePath(file.path(script_dir, ".."))
data_dir <- file.path(project_root, "Data")
output_dir <- file.path(project_root, "Base_Import_Share")
wiliam_root <- localizar_wiliam(project_root)

path_trade <- file.path(
  wiliam_root,
  "model_parameters/economy/Trade.xlsx"
)
path_pp_to_bp <- file.path(
  wiliam_root,
  "model_parameters/economy/PP_to_BP.xlsx"
)

tolerancia <- 1e-12
tolerancia_comparacion <- 1e-10
tolerancia_validacion <- 1e-4
tolerancia_flujo_pequeno <- 5e-3
tolerancia_rango <- 1e-10
tolerancia_suma_share <- 1e-9

###################
# CLASIFICACIONES
###################

entorno_sectores <- new.env(parent = emptyenv())
load(
  file.path(data_dir, "mis_sectores.RData"),
  envir = entorno_sectores
)

sectores_intermedios <- entorno_sectores$sectores_prioritarios
sectores_finales <- entorno_sectores$sectores_finales
sectores_columna <- entorno_sectores$sectores_columna

pais_orden <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "UK",
  "China", "EASOC", "India", "LATAM", "Russia", "USMCA", "LROW"
)

map_country_to_wiliam <- c(
  AUSTRIA = "AUT",
  BELGIUM = "BEL",
  BULGARIA = "BGR",
  CROATIA = "HRV",
  CYPRUS = "CYP",
  CZECHREPUBLIC = "CZE",
  DENMARK = "DNK",
  ESTONIA = "EST",
  FINLAND = "FIN",
  FRANCE = "FRA",
  GERMANY = "DEU",
  GREECE = "GRC",
  HUNGARY = "HUN",
  IRELAND = "IRL",
  ITALY = "ITA",
  LATVIA = "LVA",
  LITHUANIA = "LTU",
  LUXEMBOURG = "LUX",
  MALTA = "MLT",
  NETHERLANDS = "NLD",
  POLAND = "POL",
  PORTUGAL = "PRT",
  ROMANIA = "ROU",
  SLOVAKIA = "SVK",
  SLOVENIA = "SVN",
  SPAIN = "ESP",
  SWEDEN = "SWE",
  UK = "UK",
  CHINA = "China",
  EASOC = "EASOC",
  INDIA = "India",
  LATAM = "LATAM",
  RUSSIA = "Russia",
  USMCA = "USMCA",
  LROW = "LROW"
)

map_to_wiliam_codes <- function(x) {
  x <- str_squish(as.character(x))
  idx <- x %in% names(map_country_to_wiliam)
  x[idx] <- unname(map_country_to_wiliam[x[idx]])
  x
}

clave_bis <- function(df) {
  paste(df$Pais, df$Sector, sep = "\r")
}

ordenar_bis <- function(df) {
  df <- df %>%
    mutate(
      Pais = map_to_wiliam_codes(Pais),
      Sector = str_squish(as.character(Sector))
    )
  df$Pais <- factor(df$Pais, levels = pais_orden)
  df$Sector <- factor(df$Sector, levels = sectores_intermedios)
  df <- df %>% arrange(Pais, Sector)
  df$Pais <- as.character(df$Pais)
  df$Sector <- as.character(df$Sector)
  df
}

###################
# CARGA DE DATOS
###################

cargar_data_origin <- function(path_rdata) {
  env_tmp <- new.env(parent = emptyenv())
  load(path_rdata, envir = env_tmp)

  if (!exists("data_origin", envir = env_tmp)) {
    stop("No se encontro data_origin en ", path_rdata, ".")
  }
  get("data_origin", envir = env_tmp)
}

preparar_mrio <- function(data_origin, fuente) {
  data_bis <- as.data.frame(data_origin)
  names(data_bis)[1:2] <- c("Pais", "Sector")
  data_bis <- data_bis %>%
    filter(
      !is.na(Pais),
      !is.na(Sector),
      Sector %in% sectores_intermedios
    ) %>%
    as.data.frame()

  paises <- as.character(data_bis$Pais)
  sectores <- as.character(data_bis$Sector)
  paises_originales <- unique(paises)

  if (length(paises_originales) != length(pais_orden)) {
    stop(fuente, " no contiene los 35 paises esperados.")
  }
  if (any(table(paises) != length(sectores_intermedios))) {
    stop(fuente, " no tiene 62 sectores por pais.")
  }

  datos <- as.matrix(
    data.frame(lapply(data_bis[, -c(1, 2), drop = FALSE], as.numeric))
  )
  datos[is.na(datos)] <- 0

  nombres_columnas <- c(
    paste(paises, sectores, sep = "_"),
    paste(
      rep(paises_originales, each = length(sectores_finales)),
      rep(sectores_finales, times = length(paises_originales)),
      sep = "_"
    )
  )

  if (ncol(datos) != length(nombres_columnas)) {
    stop(fuente, " no tiene las 2.380 columnas de uso esperadas.")
  }

  colnames(datos) <- nombres_columnas

  list(
    fuente = fuente,
    paises_originales = paises,
    paises = map_to_wiliam_codes(paises),
    sectores = sectores,
    datos = datos,
    pais_destino = str_extract(nombres_columnas, "^[^_]+"),
    uso = str_remove(nombres_columnas, "^[^_]+_")
  )
}

leer_referencia_wiliam <- function(path, sheet, columnas) {
  df <- read.xlsx(path, sheet = sheet)
  names(df)[1:2] <- c("Pais", "Sector")
  df <- df %>%
    filter(!is.na(Pais), !is.na(Sector)) %>%
    mutate(
      Pais = map_to_wiliam_codes(Pais),
      Sector = as.character(Sector),
      across(
        all_of(columnas),
        ~ replace_na(as.numeric(.), 0)
      )
    ) %>%
    select(Pais, Sector, all_of(columnas))

  ordenar_bis(df)
}

###################
# CALCULO BIS
###################

calcular_bis <- function(
    mrio,
    tratamiento_negativos = c("conservar", "cero")) {
  tratamiento_negativos <- match.arg(tratamiento_negativos)
  datos_usados <- mrio$datos

  if (tratamiento_negativos == "cero") {
    datos_usados[datos_usados < 0] <- 0
  }

  n_filas <- nrow(datos_usados)
  n_usos <- length(sectores_columna)
  importaciones <- matrix(0, nrow = n_filas, ncol = n_usos)
  domestico <- importaciones
  total <- importaciones
  share <- importaciones

  colnames(importaciones) <- sectores_columna
  colnames(domestico) <- sectores_columna
  colnames(total) <- sectores_columna
  colnames(share) <- sectores_columna

  for (fila in seq_len(n_filas)) {
    pais_actual <- mrio$paises_originales[[fila]]
    sector_actual <- mrio$sectores[[fila]]
    columnas_pais <- which(mrio$pais_destino == pais_actual)
    filas_producto <- which(mrio$sectores == sector_actual)

    if (length(columnas_pais) != n_usos) {
      stop("Faltan usos para ", pais_actual, " en ", mrio$fuente, ".")
    }

    importaciones_fila <- colSums(
      datos_usados[
        setdiff(filas_producto, fila),
        columnas_pais,
        drop = FALSE
      ]
    )
    domestico_fila <- datos_usados[fila, columnas_pais]
    total_fila <- importaciones_fila + domestico_fila

    importaciones[fila, ] <- importaciones_fila
    domestico[fila, ] <- domestico_fila
    total[fila, ] <- total_fila
    share[fila, ] <- ifelse(
      abs(total_fila) <= tolerancia,
      0,
      importaciones_fila / total_fila
    )
  }

  posiciones_negativas <- which(mrio$datos < 0, arr.ind = TRUE)
  negativos <- tibble(
    Fuente = mrio$fuente,
    Pais_origen = mrio$paises[posiciones_negativas[, "row"]],
    Producto = mrio$sectores[posiciones_negativas[, "row"]],
    Pais_destino = map_to_wiliam_codes(
      mrio$pais_destino[posiciones_negativas[, "col"]]
    ),
    Uso = mrio$uso[posiciones_negativas[, "col"]],
    Tipo_flujo = if_else(
      Pais_origen == Pais_destino,
      "Domestico",
      "Importado"
    ),
    Valor_original = mrio$datos[posiciones_negativas],
    Valor_usado = datos_usados[posiciones_negativas]
  ) %>%
    mutate(Ajuste = Valor_usado - Valor_original) %>%
    arrange(Uso, Pais_destino, Producto, Pais_origen)

  wide <- bind_cols(
    tibble(Pais = mrio$paises, Sector = mrio$sectores),
    as_tibble(share, .name_repair = "minimal")
  ) %>%
    ordenar_bis()

  orden <- match(clave_bis(wide), paste(mrio$paises, mrio$sectores, sep = "\r"))

  list(
    wide = wide,
    importaciones = importaciones[orden, , drop = FALSE],
    domestico = domestico[orden, , drop = FALSE],
    total = total[orden, , drop = FALSE],
    share = share[orden, , drop = FALSE],
    negativos = negativos
  )
}

###################
# COMPROBACIONES
###################

alinear_referencia <- function(calc, ref) {
  idx <- match(clave_bis(calc), clave_bis(ref))
  if (anyNA(idx) || nrow(calc) != nrow(ref)) {
    stop("Las claves calculadas y de referencia no coinciden.")
  }
  ref[idx, , drop = FALSE]
}

comparar_wiliam <- function(resultado, referencia) {
  calc <- resultado$wide
  ref <- alinear_referencia(calc, referencia)
  calc_mat <- as.matrix(calc[, sectores_columna])
  ref_mat <- as.matrix(ref[, sectores_columna])
  diferencia_firmada <- calc_mat - ref_mat
  diferencia <- abs(diferencia_firmada)
  posiciones <- which(
    diferencia > tolerancia_comparacion,
    arr.ind = TRUE
  )

  discrepancias <- if (nrow(posiciones) == 0) {
    tibble(
      Pais = character(),
      Producto = character(),
      Uso = character(),
      Calculado_MRIO = numeric(),
      WILIAM_oficial = numeric(),
      Diferencia = numeric(),
      Diferencia_abs = numeric(),
      Importaciones = numeric(),
      Domestico = numeric(),
      Total = numeric(),
      Dentro_tolerancia = logical(),
      Flujo_total_pequeno = logical(),
      Diagnostico = character(),
      Estado = character()
    )
  } else {
    fila <- posiciones[, "row"]
    columna <- posiciones[, "col"]
    diff_val <- diferencia[posiciones]
    total_val <- resultado$total[posiciones]
    dentro_tolerancia <- diff_val <= tolerancia_validacion
    flujo_total_pequeno <- abs(total_val) <= tolerancia_flujo_pequeno

    tibble(
      Pais = calc$Pais[fila],
      Producto = calc$Sector[fila],
      Uso = sectores_columna[columna],
      Calculado_MRIO = calc_mat[posiciones],
      WILIAM_oficial = ref_mat[posiciones],
      Diferencia = diferencia_firmada[posiciones],
      Diferencia_abs = diff_val,
      Importaciones = resultado$importaciones[posiciones],
      Domestico = resultado$domestico[posiciones],
      Total = total_val,
      Dentro_tolerancia = dentro_tolerancia,
      Flujo_total_pequeno = flujo_total_pequeno,
      Diagnostico = case_when(
        dentro_tolerancia ~
          "Diferencia numerica dentro de la tolerancia de 0.0001",
        flujo_total_pequeno ~
          paste(
            "Flujo total <= 0.005: el cociente es sensible al redondeo,",
            "pero esto no demuestra igualdad con WILIAM"
          ),
        TRUE ~ paste(
          "Diferencia superior a 0.0001;",
          "revisar la fuente sin redondear o la regla usada por WILIAM"
        )
      ),
      Estado = if_else(
        dentro_tolerancia,
        "DENTRO_TOLERANCIA",
        "REVISAR"
      )
    ) %>%
      arrange(desc(Diferencia_abs))
  }

  resumen_filas <- tibble(
    Pais = calc$Pais,
    Sector = calc$Sector,
    Celdas_diferentes_numericas = rowSums(
      diferencia > tolerancia_comparacion
    ),
    Celdas_fuera_tolerancia = rowSums(
      diferencia > tolerancia_validacion
    ),
    Max_diferencia = apply(diferencia, 1, max),
    Resultado = if_else(
      Celdas_fuera_tolerancia == 0,
      "BIEN",
      "REVISAR"
    )
  )

  list(
    calculado = calc,
    referencia = ref,
    discrepancias = discrepancias,
    resumen_filas = resumen_filas,
    diferencias = diferencia
  )
}

detectar_fuera_rango <- function(df, fuente) {
  matriz <- as.matrix(df[, sectores_columna])
  posiciones <- which(
    matriz < -tolerancia_rango |
      matriz > 1 + tolerancia_rango,
    arr.ind = TRUE
  )

  if (nrow(posiciones) == 0) {
    return(tibble(
      Fuente = character(),
      Pais = character(),
      Producto = character(),
      Uso = character(),
      BIS = numeric(),
      Desviacion = numeric()
    ))
  }

  fila <- posiciones[, "row"]
  columna <- posiciones[, "col"]
  valor <- matriz[posiciones]

  tibble(
    Fuente = fuente,
    Pais = df$Pais[fila],
    Producto = df$Sector[fila],
    Uso = sectores_columna[columna],
    BIS = valor,
    Desviacion = pmax(-valor, valor - 1)
  ) %>%
    arrange(desc(Desviacion))
}

check_suma_share_bis <- function(resultado, fuente) {
  calc <- resultado$wide
  total <- resultado$total
  importaciones <- resultado$importaciones
  domestico <- resultado$domestico
  share_importada <- as.matrix(calc[, sectores_columna])
  share_domestica <- ifelse(
    abs(total) <= tolerancia,
    0,
    domestico / total
  )
  suma_share <- share_importada + share_domestica
  esperado <- ifelse(abs(total) <= tolerancia, 0, 1)
  error_abs <- abs(suma_share - esperado)
  posiciones <- which(
    matrix(TRUE, nrow = nrow(calc), ncol = length(sectores_columna)),
    arr.ind = TRUE
  )
  fila <- posiciones[, "row"]
  columna <- posiciones[, "col"]

  tibble(
    Fuente = fuente,
    Pais = calc$Pais[fila],
    Producto = calc$Sector[fila],
    Uso = sectores_columna[columna],
    Importaciones = importaciones[posiciones],
    Domestico = domestico[posiciones],
    Total = total[posiciones],
    BIS = share_importada[posiciones],
    Share_domestica = share_domestica[posiciones],
    Suma_share = suma_share[posiciones],
    Esperado = esperado[posiciones],
    Error_abs = error_abs[posiciones],
    Check_suma_share = if_else(
      Error_abs <= tolerancia_suma_share,
      "BIEN",
      "REVISAR"
    )
  )
}

###################
# EJECUCION WILIAM
###################

data_origin_wiliam <- cargar_data_origin(
  file.path(data_dir, "Data_origin_WILIAM.RData")
)
data_origin_unizar <- cargar_data_origin(
  file.path(data_dir, "Data_origin_UNIZAR.RData")
)

mrio_wiliam <- preparar_mrio(
  data_origin_wiliam,
  "Data_origin_WILIAM.RData"
)
mrio_unizar <- preparar_mrio(
  data_origin_unizar,
  "Data_origin_UNIZAR.RData"
)

referencia_trade <- leer_referencia_wiliam(
  path_trade,
  "BASE_Import_shares",
  sectores_columna
)
referencia_pp <- leer_referencia_wiliam(
  path_pp_to_bp,
  "BASE_Import_shares_PP",
  sectores_finales
)

resultado_wiliam_raw <- calcular_bis(
  mrio_wiliam,
  tratamiento_negativos = "conservar"
)
validacion_wiliam <- comparar_wiliam(
  resultado_wiliam_raw,
  referencia_trade
)

negativos_wiliam <- resultado_wiliam_raw$negativos %>%
  mutate(
    Explicacion = case_when(
      Uso == "CHANGE_IN_INVENTORIES_AND_VALUABLES" ~
        paste(
          "Desacumulacion o retirada de existencias;",
          "signo contable esperado"
        ),
      abs(Valor_original) <= 5e-4 ~
        "Residuo de redondeo de la MRIO a 4 decimales",
      TRUE ~ "Negativo fuera de variaciones de existencias: revisar"
    )
  )

referencia_pp <- alinear_referencia(
  validacion_wiliam$calculado,
  referencia_pp
)

# WILIAM carga los intermedios desde Trade.xlsx y la demanda final desde
# PP_to_BP.xlsx. Esta tabla representa las fuentes efectivas del modelo.
bis_wiliam_modelo <- validacion_wiliam$referencia
bis_wiliam_modelo[, sectores_finales] <- referencia_pp[, sectores_finales]

diff_pp_bp <- abs(
  as.matrix(referencia_pp[, sectores_finales]) -
    as.matrix(
      validacion_wiliam$referencia[, sectores_finales]
    )
)

diff_wiliam <- validacion_wiliam$diferencias
n_diff_intermedios <- sum(
  diff_wiliam[, sectores_intermedios, drop = FALSE] >
    tolerancia_comparacion
)
n_fuera_intermedios <- sum(
  diff_wiliam[, sectores_intermedios, drop = FALSE] >
    tolerancia_validacion
)
n_diff_finales <- sum(
  diff_wiliam[, sectores_finales, drop = FALSE] >
    tolerancia_comparacion
)
n_fuera_finales <- sum(
  diff_wiliam[, sectores_finales, drop = FALSE] >
    tolerancia_validacion
)

fuentes_wiliam <- tibble(
  Bloque = c(
    "Intermedios (62)",
    "Demanda final a precios basicos (6)",
    "Demanda final usada por el modelo (6)"
  ),
  Calculo_o_fuente = c(
    "Data_origin_WILIAM.RData",
    "Data_origin_WILIAM.RData",
    "PP_to_BP.xlsx / BASE_Import_shares_PP"
  ),
  Referencia = c(
    "Trade.xlsx / BASE_Import_shares",
    "Trade.xlsx / BASE_Import_shares",
    "WILIAM.mdl / INITIAL_IMPORT_SHARES_FINAL_DEMAND"
  ),
  Celdas_diferentes_numericas = c(
    n_diff_intermedios,
    n_diff_finales,
    sum(diff_pp_bp > tolerancia_comparacion)
  ),
  Celdas_fuera_tolerancia = c(
    n_fuera_intermedios,
    n_fuera_finales,
    sum(diff_pp_bp > tolerancia_validacion)
  ),
  Max_diferencia = c(
    max(
      validacion_wiliam$diferencias[
        , sectores_intermedios,
        drop = FALSE
      ]
    ),
    max(
      validacion_wiliam$diferencias[
        , sectores_finales,
        drop = FALSE
      ]
    ),
    max(diff_pp_bp)
  ),
  Explicacion = c(
    paste(
      "El calculo usa la formula indicada sin sustituir valores.",
      n_fuera_intermedios,
      "celdas superan la tolerancia de 0.0001."
    ),
    paste(
      "Las diferencias de demanda final a precios basicos",
      "quedan dentro de la tolerancia de 0.0001."
    ),
    paste(
      "El modelo transforma la demanda final a precios de comprador;",
      "por eso no debe compararse directamente con la MRIO basica."
    )
  ),
  Resultado = c(
    if_else(n_fuera_intermedios == 0, "BIEN", "REVISAR"),
    if_else(n_fuera_finales == 0, "BIEN", "REVISAR"),
    "DOCUMENTADO"
  )
)

###################
# EJECUCION UNIZAR
###################

resultado_unizar_raw <- calcular_bis(
  mrio_unizar,
  tratamiento_negativos = "conservar"
)
resultado_unizar_ajustado <- calcular_bis(
  mrio_unizar,
  tratamiento_negativos = "cero"
)

fuera_rango_wiliam_bp <- detectar_fuera_rango(
  validacion_wiliam$referencia,
  "WILIAM oficial a precios basicos"
)
fuera_rango_wiliam_modelo <- detectar_fuera_rango(
  bis_wiliam_modelo,
  "WILIAM fuentes efectivas del modelo"
)
fuera_rango_unizar_raw <- detectar_fuera_rango(
  resultado_unizar_raw$wide,
  "UNIZAR sin tratar negativos"
)
fuera_rango_unizar_ajustado <- detectar_fuera_rango(
  resultado_unizar_ajustado$wide,
  "UNIZAR ajustado"
)
check_suma_unizar_ajustado <- check_suma_share_bis(
  resultado_unizar_ajustado,
  "UNIZAR ajustado"
)

grupos_unizar_activos <- check_suma_unizar_ajustado %>%
  filter(Esperado == 1)
grupos_unizar_cero <- check_suma_unizar_ajustado %>%
  filter(Esperado == 0)

negativos_unizar <- resultado_unizar_ajustado$negativos %>%
  mutate(
    Explicacion = if_else(
      Uso == "CHANGE_IN_INVENTORIES_AND_VALUABLES",
      paste(
        "Desacumulacion o retirada de existencias;",
        "no es una importacion fisica negativa"
      ),
      "Revisar: negativo fuera de variaciones de existencias"
    )
  )

if (any(
  negativos_unizar$Uso !=
    "CHANGE_IN_INVENTORIES_AND_VALUABLES"
)) {
  stop("UNIZAR contiene negativos fuera de variaciones de existencias.")
}

mat_unizar_raw <- as.matrix(
  resultado_unizar_raw$wide[, sectores_columna]
)
mat_unizar_ajustado <- as.matrix(
  resultado_unizar_ajustado$wide[, sectores_columna]
)
total_celdas <- length(mat_unizar_ajustado)
ceros_unizar <- sum(mat_unizar_ajustado == 0)
celdas_modificadas <- sum(
  abs(mat_unizar_raw - mat_unizar_ajustado) >
    tolerancia_comparacion
)

resumen_checks <- tibble(
  Chequeo = c(
    "Coincidencia numerica WILIAM frente a Trade",
    "Coincidencia WILIAM con tolerancia absoluta de 0.0001",
    "Rango [0,1] de WILIAM oficial a precios basicos",
    "Rango [0,1] de WILIAM con fuentes efectivas del modelo",
    "Negativos UNIZAR limitados a variaciones de existencias",
    "Suma de shares BIS UNIZAR ajustado en grupos con flujo",
    "Suma de shares BIS UNIZAR ajustado en grupos sin flujo",
    "Rango [0,1] de UNIZAR sin tratar negativos",
    "Rango [0,1] de UNIZAR ajustado",
    "Ceros UNIZAR conservados exactamente"
  ),
  Total = c(
    length(validacion_wiliam$diferencias),
    length(validacion_wiliam$diferencias),
    length(as.matrix(
      validacion_wiliam$referencia[, sectores_columna]
    )),
    length(as.matrix(bis_wiliam_modelo[, sectores_columna])),
    nrow(negativos_unizar),
    nrow(grupos_unizar_activos),
    nrow(grupos_unizar_cero),
    total_celdas,
    total_celdas,
    ceros_unizar
  ),
  Bien = c(
    sum(
      validacion_wiliam$diferencias <=
        tolerancia_comparacion
    ),
    sum(
      validacion_wiliam$diferencias <=
        tolerancia_validacion
    ),
    total_celdas - nrow(fuera_rango_wiliam_bp),
    total_celdas - nrow(fuera_rango_wiliam_modelo),
    sum(
      negativos_unizar$Uso ==
        "CHANGE_IN_INVENTORIES_AND_VALUABLES"
    ),
    sum(grupos_unizar_activos$Check_suma_share == "BIEN"),
    sum(grupos_unizar_cero$Check_suma_share == "BIEN"),
    total_celdas - nrow(fuera_rango_unizar_raw),
    total_celdas - nrow(fuera_rango_unizar_ajustado),
    ceros_unizar
  ),
  Revisar = c(
    sum(
      validacion_wiliam$diferencias >
        tolerancia_comparacion
    ),
    sum(
      validacion_wiliam$diferencias >
        tolerancia_validacion
    ),
    nrow(fuera_rango_wiliam_bp),
    nrow(fuera_rango_wiliam_modelo),
    sum(
      negativos_unizar$Uso !=
        "CHANGE_IN_INVENTORIES_AND_VALUABLES"
    ),
    sum(grupos_unizar_activos$Check_suma_share == "REVISAR"),
    sum(grupos_unizar_cero$Check_suma_share == "REVISAR"),
    nrow(fuera_rango_unizar_raw),
    nrow(fuera_rango_unizar_ajustado),
    0
  ),
  Resultado = c(
    if_else(
      any(
        validacion_wiliam$diferencias >
          tolerancia_comparacion
      ),
      "REVISAR",
      "BIEN"
    ),
    if_else(
      any(
        validacion_wiliam$diferencias >
          tolerancia_validacion
      ),
      "REVISAR",
      "BIEN"
    ),
    if_else(
      nrow(fuera_rango_wiliam_bp) == 0,
      "BIEN",
      "DOCUMENTADO"
    ),
    if_else(
      nrow(fuera_rango_wiliam_modelo) == 0,
      "BIEN",
      "DOCUMENTADO"
    ),
    "BIEN",
    if_else(
      all(grupos_unizar_activos$Check_suma_share == "BIEN"),
      "BIEN",
      "REVISAR"
    ),
    if_else(
      all(grupos_unizar_cero$Check_suma_share == "BIEN"),
      "BIEN",
      "REVISAR"
    ),
    if_else(
      nrow(fuera_rango_unizar_raw) == 0,
      "BIEN",
      "DOCUMENTADO"
    ),
    if_else(
      nrow(fuera_rango_unizar_ajustado) == 0,
      "BIEN",
      "REVISAR"
    ),
    "BIEN"
  )
)

explicacion_wiliam <- tibble(
  Concepto = c(
    "Formula aplicada",
    "Resultado de la comparacion",
    "Fallo de la comprobacion historica",
    "Interpretacion de los flujos pequenos",
    "Decision metodologica"
  ),
  Explicacion = c(
    paste(
      "BIS = importaciones / (domestico + importaciones),",
      "calculado directamente desde Data_origin_WILIAM.RData."
    ),
    paste(
      nrow(validacion_wiliam$discrepancias),
      "celdas difieren numericamente de Trade.xlsx y",
      sum(!validacion_wiliam$discrepancias$Dentro_tolerancia),
      "superan la tolerancia absoluta de 0.0001."
    ),
    paste(
      "La hoja antigua usaba OR(diferencia < 0.0001,",
      "-0.0001 > diferencia). Esa expresion acepta cualquier",
      "diferencia negativa. La comprobacion correcta usa ABS(diferencia)."
    ),
    paste(
      "Un total muy pequeno hace que el cociente cambie mucho con",
      "redondeos minimos. Es una posible causa, no una prueba de igualdad."
    ),
    paste(
      "Los valores calculados no se reemplazan por los oficiales.",
      "Para certificar igualdad exacta hace falta la matriz sin redondear",
      "o la regla original con la que se genero Trade.xlsx."
    )
  )
)

explicacion_negativos <- tibble(
  Concepto = c(
    "Que significa un valor negativo",
    "Por que aparece",
    "Por que el BIS sale fuera de [0,1]",
    "Que hace WILIAM oficial",
    "Que hace la version UNIZAR ajustada",
    "Limitacion metodologica"
  ),
  Explicacion = c(
    paste(
      "En CHANGE_IN_INVENTORIES_AND_VALUABLES un valor negativo",
      "representa retirada o desacumulacion de existencias."
    ),
    paste(
      "Las tablas input-output registran la variacion neta del stock;",
      "si se usa mas inventario del que se acumula, el flujo es negativo."
    ),
    paste(
      "BIS = importaciones / (domestico + importaciones).",
      "Con flujos negativos el denominador puede ser negativo o casi cero,",
      "y el cociente deja de ser una proporcion."
    ),
    paste(
      "Las fuentes oficiales conservan estos valores netos.",
      "Por eso WILIAM tambien contiene coeficientes fuera de [0,1]",
      "en variaciones de existencias. Data_origin_WILIAM contiene",
      nrow(negativos_wiliam),
      "flujos negativos:",
      sum(
        negativos_wiliam$Uso ==
          "CHANGE_IN_INVENTORIES_AND_VALUABLES"
      ),
      "son existencias y uno es un residuo de redondeo de -0.0001."
    ),
    paste(
      "Para obtener una share acotada, los flujos negativos se sustituyen",
      "por cero antes de sumar importaciones y produccion domestica."
    ),
    paste(
      "El ajuste a cero es una convencion para construir una proporcion",
      "de suministro positivo; no es una reproduccion literal de WILIAM."
    )
  )
)

metodologia <- tibble(
  Paso = c(
    "1. Reproduccion WILIAM",
    "2. Referencia oficial a precios basicos",
    "3. Fuente efectiva para demanda final",
    "4. Aplicacion UNIZAR sin ajuste",
    "5. Aplicacion UNIZAR ajustada",
    "6. Comprobacion de suma",
    "7. Salida operativa"
  ),
  Descripcion = c(
    paste(
      "Se calcula BIS desde Data_origin_WILIAM.RData con la formula",
      "importaciones / (domestico + importaciones)."
    ),
    paste(
      "Se compara celda a celda con Trade.xlsx / BASE_Import_shares.",
      nrow(validacion_wiliam$discrepancias),
      "celdas difieren numericamente y",
      sum(!validacion_wiliam$discrepancias$Dentro_tolerancia),
      "superan la tolerancia de 0.0001. No se sustituyen valores."
    ),
    paste(
      "WILIAM.mdl carga los 6 usos finales desde",
      "PP_to_BP.xlsx / BASE_Import_shares_PP."
    ),
    paste(
      "Se aplica la misma formula a Data_origin_UNIZAR.RData",
      "conservando los signos para diagnostico."
    ),
    paste(
      nrow(negativos_unizar),
      "flujos negativos de variaciones de existencias se ponen a cero;",
      celdas_modificadas,
      "coeficientes cambian."
    ),
    paste(
      "Se comprueba que BIS importado + share domestica suma 1",
      "cuando existe flujo total, y suma 0 cuando no existe flujo."
    ),
    paste(
      "Base_Import_Share_R.xlsx contiene la version UNIZAR ajustada;",
      ceros_unizar,
      "coeficientes permanecen exactamente en cero."
    )
  )
)

###################
# EXPORTACION
###################

# Se conserva el formato historico del fichero operativo: 2.170 x 68 sin IDs.
write.xlsx(
  as.data.frame(mat_unizar_ajustado),
  file.path(output_dir, "Base_Import_Share_R.xlsx"),
  overwrite = TRUE
)

hojas <- list(
  RESUMEN_CHECKS = resumen_checks,
  EXPLICACION_WILIAM = explicacion_wiliam,
  DISCREPANCIAS_WILIAM = validacion_wiliam$discrepancias,
  COMPROBACION_WILIAM = validacion_wiliam$resumen_filas,
  FUENTES_WILIAM = fuentes_wiliam,
  BIS_R_WILIAM = validacion_wiliam$calculado,
  BIS_WILIAM_TRADE = validacion_wiliam$referencia,
  BIS_WILIAM_MODELO = bis_wiliam_modelo,
  BIS_UNIZAR_RAW = resultado_unizar_raw$wide,
  BIS_UNIZAR_AJUSTADO = resultado_unizar_ajustado$wide,
  CHECK_SUMA_SHARE_UNIZAR = check_suma_unizar_ajustado,
  FUERA_RANGO_WILIAM_BP = fuera_rango_wiliam_bp,
  FUERA_RANGO_WILIAM_MODELO = fuera_rango_wiliam_modelo,
  FUERA_RANGO_UNIZAR_RAW = fuera_rango_unizar_raw,
  NEGATIVOS_WILIAM = negativos_wiliam,
  AJUSTES_NEGATIVOS_UNIZAR = negativos_unizar,
  EXPLICACION_NEGATIVOS = explicacion_negativos,
  METODOLOGIA = metodologia
)

wb <- createWorkbook()
estilo_cabecera <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#1F4E78",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)
estilo_bien <- createStyle(
  fgFill = "#C6EFCE",
  fontColour = "#006100"
)
estilo_revisar <- createStyle(
  fgFill = "#FFC7CE",
  fontColour = "#9C0006"
)
estilo_documentado <- createStyle(
  fgFill = "#FFEB9C",
  fontColour = "#9C6500"
)
estilo_texto_largo <- createStyle(
  wrapText = TRUE,
  valign = "top"
)

for (nombre_hoja in names(hojas)) {
  datos_hoja <- as.data.frame(hojas[[nombre_hoja]])
  addWorksheet(wb, nombre_hoja)
  writeData(wb, nombre_hoja, datos_hoja, withFilter = TRUE)
  addStyle(
    wb,
    nombre_hoja,
    estilo_cabecera,
    rows = 1,
    cols = seq_len(ncol(datos_hoja)),
    gridExpand = TRUE
  )
  freezePane(wb, nombre_hoja, firstRow = TRUE, firstCol = TRUE)
  setRowHeights(wb, nombre_hoja, rows = 1, heights = 32)

  if (all(c("Pais", "Sector") %in% names(datos_hoja))) {
    setColWidths(wb, nombre_hoja, cols = 1, widths = 15)
    setColWidths(wb, nombre_hoja, cols = 2, widths = 42)
    if (ncol(datos_hoja) > 2) {
      setColWidths(
        wb,
        nombre_hoja,
        cols = 3:ncol(datos_hoja),
        widths = 14
      )
    }
  } else {
    setColWidths(
      wb,
      nombre_hoja,
      cols = seq_len(ncol(datos_hoja)),
      widths = "auto"
    )
  }

  if ("Explicacion" %in% names(datos_hoja)) {
    col_explicacion <- match("Explicacion", names(datos_hoja))
    setColWidths(wb, nombre_hoja, cols = col_explicacion, widths = 90)
    addStyle(
      wb,
      nombre_hoja,
      estilo_texto_largo,
      rows = 2:(nrow(datos_hoja) + 1),
      cols = col_explicacion,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if ("Diagnostico" %in% names(datos_hoja)) {
    col_diagnostico <- match("Diagnostico", names(datos_hoja))
    setColWidths(wb, nombre_hoja, cols = col_diagnostico, widths = 70)
    addStyle(
      wb,
      nombre_hoja,
      estilo_texto_largo,
      rows = 2:(nrow(datos_hoja) + 1),
      cols = col_diagnostico,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if ("Chequeo" %in% names(datos_hoja)) {
    setColWidths(
      wb,
      nombre_hoja,
      cols = match("Chequeo", names(datos_hoja)),
      widths = 58
    )
  }
}

for (nombre_hoja in names(hojas)) {
  datos_hoja <- as.data.frame(hojas[[nombre_hoja]])
  columna_estado <- intersect(
    c("Resultado", "Estado"),
    names(datos_hoja)
  )

  if (length(columna_estado) == 0) {
    next
  }

  col_estado <- match(columna_estado[[1]], names(datos_hoja))
  valores_estado <- datos_hoja[[columna_estado[[1]]]]

  for (estado in c(
    "BIEN",
    "REVISAR",
    "DOCUMENTADO",
    "DENTRO_TOLERANCIA"
  )) {
    filas <- which(valores_estado == estado) + 1
    if (length(filas) == 0) {
      next
    }
    estilo <- switch(
      estado,
      BIEN = estilo_bien,
      REVISAR = estilo_revisar,
      DOCUMENTADO = estilo_documentado,
      DENTRO_TOLERANCIA = estilo_documentado
    )
    addStyle(
      wb,
      nombre_hoja,
      estilo,
      rows = filas,
      cols = col_estado,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

saveWorkbook(
  wb,
  file.path(output_dir, "Comprobaciones_base_import_share.xlsx"),
  overwrite = TRUE
)

print(resumen_checks)
print(fuentes_wiliam)
