suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
  library(stringr)
  library(tibble)
})

###################
# RUTAS Y PARAMETROS
###################

obtener_directorio_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg[[1]]),
      mustWork = FALSE
    )))
  }

  cwd <- normalizePath(getwd())
  if (basename(cwd) == "Coeficientes_tecnicos") {
    return(cwd)
  }
  normalizePath(file.path(cwd, "Coeficientes_tecnicos"))
}

localizar_wiliam <- function(project_root) {
  candidatos <- c(file.path(dirname(project_root), "WILIAM"), project_root)
  valido <- candidatos[
    file.exists(file.path(
      candidatos,
      "model_parameters/economy/Production.xlsx"
    ))
  ]

  if (length(valido) == 0) {
    stop("No se encontro el repositorio WILIAM con Production.xlsx.")
  }
  normalizePath(valido[[1]])
}

script_dir <- obtener_directorio_script()
project_root <- normalizePath(file.path(script_dir, ".."))
data_dir <- file.path(project_root, "Data")
output_dir <- script_dir
checks_dir <- file.path(output_dir, "Comprobaciones")
dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)

wiliam_root <- localizar_wiliam(project_root)
path_production <- file.path(
  wiliam_root,
  "model_parameters/economy/Production.xlsx"
)

tolerancia_cero <- 1e-12
tolerancia_estricta <- 1e-10
tolerancia_mrio_redondeada <- 2e-6
tolerancia_rango <- 1e-12

###################
# CLASIFICACIONES
###################

entorno_sectores <- new.env(parent = emptyenv())
load(file.path(data_dir, "mis_sectores.RData"), envir = entorno_sectores)
sectores <- entorno_sectores$sectores_prioritarios

if (length(sectores) != 62) {
  stop("Se esperaban 62 sectores intermedios.")
}

pais_orden_codigos <- c(
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
  CZECH_REPUBLIC = "CZE",
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
  out <- str_squish(as.character(x))
  idx <- out %in% names(map_country_to_wiliam)
  out[idx] <- unname(map_country_to_wiliam[out[idx]])
  out
}

###################
# CARGA Y CALCULO
###################

cargar_data_origin <- function(path_rdata) {
  env_tmp <- new.env(parent = emptyenv())
  load(path_rdata, envir = env_tmp)

  if (!exists("data_origin", envir = env_tmp)) {
    stop("No se encontro data_origin en ", path_rdata, ".")
  }
  as.data.frame(get("data_origin", envir = env_tmp))
}

como_matriz_numerica <- function(df) {
  out <- as.matrix(data.frame(lapply(df, as.numeric), check.names = FALSE))
  out[is.na(out)] <- 0
  out
}

calcular_tc_mrio <- function(data_origin, fuente) {
  names(data_origin)[1:2] <- c("Pais", "Sector")
  if (nrow(data_origin) < 2206 || ncol(data_origin) < 2172) {
    stop(fuente, " no tiene la forma MRIO esperada.")
  }

  mrio <- data_origin[1:2206, , drop = FALSE]
  valores_intermedios <- como_matriz_numerica(
    mrio[, 3:(2 + 35 * 62), drop = FALSE]
  )
  denominador <- colSums(valores_intermedios)

  numerador <- vapply(
    sectores,
    function(sector) {
      colSums(
        valores_intermedios[
          as.character(mrio$Sector) == sector,
          ,
          drop = FALSE
        ]
      )
    },
    numeric(ncol(valores_intermedios))
  )
  numerador <- t(numerador)
  rownames(numerador) <- sectores

  tc_wide <- sweep(numerador, 2, denominador, "/")
  tc_wide[, abs(denominador) <= tolerancia_cero] <- 0
  tc_wide[!is.finite(tc_wide)] <- 0

  paises_originales <- unique(as.character(mrio$Pais[1:(35 * 62)]))
  if (length(paises_originales) != 35) {
    stop(fuente, " no contiene los 35 paises esperados.")
  }

  nombres_columnas <- unlist(
    lapply(paises_originales, function(pais) paste(pais, sectores, sep = "_")),
    use.names = FALSE
  )
  colnames(tc_wide) <- nombres_columnas

  tc_final <- matrix(
    0,
    nrow = 35 * 62,
    ncol = 62,
    dimnames = list(NULL, sectores)
  )
  for (pais_idx in seq_len(35)) {
    filas <- ((pais_idx - 1) * 62 + 1):(pais_idx * 62)
    columnas <- filas
    tc_final[filas, ] <- tc_wide[, columnas, drop = FALSE]
  }

  ids_originales <- tibble(
    Country = rep(paises_originales, each = 62),
    Text = rep(sectores, times = 35)
  )
  ids_codigos <- ids_originales %>%
    mutate(Country = map_to_wiliam_codes(Country))

  list(
    fuente = fuente,
    wide = tc_wide,
    final = tc_final,
    ids_originales = ids_originales,
    ids_codigos = ids_codigos,
    denominador = denominador
  )
}

calcular_tecnologia_media <- function(raw) {
  promedio <- matrix(
    0,
    nrow = 62,
    ncol = 62,
    dimnames = list(sectores, sectores)
  )

  for (sector_entrada_idx in seq_len(62)) {
    filas_paises <- seq(sector_entrada_idx, 35 * 62, by = 62)
    for (sector_destino_idx in seq_len(62)) {
      valores <- raw[filas_paises, sector_destino_idx]
      valores <- valores[!is.na(valores) & valores != 0]
      promedio[sector_entrada_idx, sector_destino_idx] <- if (
        length(valores) == 0
      ) {
        0
      } else {
        mean(valores)
      }
    }
  }

  # Regla explicita del libro Production.xlsx de WILIAM:
  # hydrogen usa la tecnologia media de manufacture chemical.
  promedio[, "HYDROGEN_PRODUCTION"] <-
    promedio[, "MANUFACTURE_CHEMICAL"]
  promedio
}

aplicar_tecnologia_media <- function(raw, ids) {
  promedio <- calcular_tecnologia_media(raw)
  ajustado <- raw
  columnas <- list()

  for (pais_idx in seq_len(35)) {
    filas <- ((pais_idx - 1) * 62 + 1):(pais_idx * 62)
    for (sector_destino_idx in seq_len(62)) {
      columna_pais <- raw[filas, sector_destino_idx]
      if (sum(abs(columna_pais), na.rm = TRUE) <= tolerancia_cero) {
        ajustado[filas, sector_destino_idx] <-
          promedio[, sector_destino_idx]
        columnas[[length(columnas) + 1]] <- tibble(
          Country = ids$Country[filas[[1]]],
          Sector_destino = sectores[[sector_destino_idx]],
          Suma_TC_antes = sum(columna_pais, na.rm = TRUE),
          Metodo = if (
            sectores[[sector_destino_idx]] == "HYDROGEN_PRODUCTION"
          ) {
            "Tecnologia media de MANUFACTURE_CHEMICAL (regla WILIAM)"
          } else {
            "Tecnologia media del mismo sector, excluyendo ceros"
          }
        )
      }
    }
  }

  columnas <- if (length(columnas) == 0) {
    tibble(
      Country = character(),
      Sector_destino = character(),
      Suma_TC_antes = numeric(),
      Metodo = character()
    )
  } else {
    bind_rows(columnas)
  }

  list(matriz = ajustado, promedio = promedio, columnas = columnas)
}

matriz_a_tabla <- function(ids, matriz) {
  bind_cols(ids, as_tibble(matriz, .name_repair = "minimal"))
}

###################
# REFERENCIA WILIAM
###################

leer_tc_wiliam <- function(sheet) {
  df <- read.xlsx(path_production, sheet = sheet, check.names = FALSE)
  names(df)[1:2] <- c("Country", "Text")
  df <- df %>%
    filter(!is.na(Country), !is.na(Text)) %>%
    mutate(
      Country = map_to_wiliam_codes(Country),
      Text = as.character(Text)
    )

  clave_ref <- paste(df$Country, df$Text, sep = "\r")
  clave_esperada <- paste(
    rep(pais_orden_codigos, each = 62),
    rep(sectores, times = 35),
    sep = "\r"
  )
  idx <- match(clave_esperada, clave_ref)
  if (anyNA(idx)) {
    stop("La referencia WILIAM no contiene todas las claves esperadas.")
  }
  df <- df[idx, , drop = FALSE]

  matriz <- como_matriz_numerica(df[, sectores, drop = FALSE])
  colnames(matriz) <- sectores
  list(
    ids = df[, c("Country", "Text"), drop = FALSE],
    matriz = matriz
  )
}

comparar_matrices <- function(
    nombre,
    calculado,
    referencia,
    ids,
    tolerancia_validacion) {
  if (!identical(dim(calculado), dim(referencia))) {
    stop("No coinciden las dimensiones en ", nombre, ".")
  }

  diferencia_firmada <- calculado - referencia
  diferencia_abs <- abs(diferencia_firmada)
  posiciones <- which(
    diferencia_abs > tolerancia_estricta,
    arr.ind = TRUE
  )

  detalle <- if (nrow(posiciones) == 0) {
    tibble(
      Country = character(),
      Text = character(),
      Sector_destino = character(),
      Calculado = numeric(),
      Referencia = numeric(),
      Diferencia = numeric(),
      Diferencia_abs = numeric(),
      Dentro_tolerancia = character()
    )
  } else {
    tibble(
      Country = ids$Country[posiciones[, "row"]],
      Text = ids$Text[posiciones[, "row"]],
      Sector_destino = colnames(calculado)[posiciones[, "col"]],
      Calculado = calculado[posiciones],
      Referencia = referencia[posiciones],
      Diferencia = diferencia_firmada[posiciones],
      Diferencia_abs = diferencia_abs[posiciones],
      Dentro_tolerancia = if_else(
        diferencia_abs[posiciones] <= tolerancia_validacion,
        "SI",
        "NO"
      )
    ) %>%
      arrange(desc(Diferencia_abs))
  }

  resumen <- tibble(
    Chequeo = nombre,
    Total_celdas = length(diferencia_abs),
    Diferencias_estrictas = sum(
      diferencia_abs > tolerancia_estricta,
      na.rm = TRUE
    ),
    Fuera_tolerancia = sum(
      diferencia_abs > tolerancia_validacion,
      na.rm = TRUE
    ),
    Max_diferencia_abs = max(diferencia_abs, na.rm = TRUE),
    Tolerancia_aplicada = tolerancia_validacion,
    Check = if_else(
      all(diferencia_abs <= tolerancia_validacion),
      "BIEN",
      "REVISAR"
    )
  )

  list(resumen = resumen, detalle = detalle)
}

###################
# EJECUCION
###################

data_wiliam <- cargar_data_origin(
  file.path(data_dir, "Data_origin_WILIAM.RData")
)
data_unizar <- cargar_data_origin(
  file.path(data_dir, "Data_origin_UNIZAR.RData")
)

tc_wiliam <- calcular_tc_mrio(data_wiliam, "Data_origin_WILIAM.RData")
tc_unizar <- calcular_tc_mrio(data_unizar, "Data_origin_UNIZAR.RData")

ref_wiliam_raw <- leer_tc_wiliam("EXO_Technical_coefficients_1")
ref_wiliam_final <- leer_tc_wiliam("EXO_Technical_coefficients")

metodo_sobre_ref <- aplicar_tecnologia_media(
  ref_wiliam_raw$matriz,
  ref_wiliam_raw$ids
)
metodo_sobre_mrio <- aplicar_tecnologia_media(
  tc_wiliam$final,
  tc_wiliam$ids_codigos
)
tc_unizar_final <- aplicar_tecnologia_media(
  tc_unizar$final,
  tc_unizar$ids_originales
)

comparacion_raw <- comparar_matrices(
  "WILIAM MRIO bruta vs EXO_Technical_coefficients_1",
  tc_wiliam$final,
  ref_wiliam_raw$matriz,
  tc_wiliam$ids_codigos,
  tolerancia_mrio_redondeada
)
comparacion_metodo <- comparar_matrices(
  "Metodo WILIAM reproducido desde referencia bruta vs referencia final",
  metodo_sobre_ref$matriz,
  ref_wiliam_final$matriz,
  ref_wiliam_final$ids,
  tolerancia_estricta
)
comparacion_final_mrio <- comparar_matrices(
  "WILIAM desde RData redondeado y tecnologia media vs referencia final",
  metodo_sobre_mrio$matriz,
  ref_wiliam_final$matriz,
  tc_wiliam$ids_codigos,
  tolerancia_mrio_redondeada
)

mat_unizar_raw <- tc_unizar$final
mat_unizar_final <- tc_unizar_final$matriz
fuera_rango_unizar <- which(
  mat_unizar_final < -tolerancia_rango |
    mat_unizar_final > 1 + tolerancia_rango,
  arr.ind = TRUE
)

resumen_unizar <- tibble(
  Chequeo = c(
    "UNIZAR: valores no finitos en matriz final",
    "UNIZAR: coeficientes fuera de [0,1]",
    "UNIZAR: columnas pais-sector completadas con tecnologia media"
  ),
  Total_celdas = c(
    length(mat_unizar_final),
    length(mat_unizar_final),
    35 * 62
  ),
  Diferencias_estrictas = c(
    sum(!is.finite(mat_unizar_final)),
    nrow(fuera_rango_unizar),
    nrow(tc_unizar_final$columnas)
  ),
  Fuera_tolerancia = c(
    sum(!is.finite(mat_unizar_final)),
    nrow(fuera_rango_unizar),
    0
  ),
  Max_diferencia_abs = c(
    0,
    if (nrow(fuera_rango_unizar) == 0) {
      0
    } else {
      max(
        pmax(
          -mat_unizar_final[fuera_rango_unizar],
          mat_unizar_final[fuera_rango_unizar] - 1
        )
      )
    },
    max(abs(mat_unizar_final - mat_unizar_raw))
  ),
  Tolerancia_aplicada = c(0, tolerancia_rango, NA_real_),
  Check = c(
    if_else(all(is.finite(mat_unizar_final)), "BIEN", "REVISAR"),
    if_else(nrow(fuera_rango_unizar) == 0, "BIEN", "REVISAR"),
    "DOCUMENTADO"
  )
)

resumen_checks <- bind_rows(
  comparacion_raw$resumen,
  comparacion_metodo$resumen,
  comparacion_final_mrio$resumen,
  resumen_unizar
)

metodologia <- tibble(
  Paso = 1:10,
  Descripcion = c(
    "Se usan las 2.206 filas MRIO para calcular el output total de cada columna.",
    "El numerador agrupa las filas de los 62 sectores intermedios.",
    "Solo se exportan las 2.170 columnas intermedias; la demanda final no es una matriz tecnologica.",
    "Si el output de una columna es cero, su coeficiente bruto se fija en cero.",
    "La matriz bruta WILIAM se compara con EXO_Technical_coefficients_1.",
    "Para un pais-sector sin produccion se usa la tecnologia media del mismo sector entre paises, excluyendo ceros.",
    "HYDROGEN_PRODUCTION usa la tecnologia media de MANUFACTURE_CHEMICAL, igual que Production.xlsx.",
    "La reproduccion del metodo se valida primero usando la matriz bruta oficial de WILIAM.",
    "Las diferencias del RData WILIAM se muestran y se evaluan en valor absoluto; no se sustituyen silenciosamente.",
    "El mismo metodo validado se aplica a UNIZAR y se comprueba el rango [0,1]."
  )
)

###################
# EXPORTAR RESULTADOS
###################

final_matrix_export <- bind_cols(
  tibble(Text = sectores),
  as_tibble(tc_unizar$wide, .name_repair = "minimal")
)
write.xlsx(
  final_matrix_export,
  file.path(output_dir, "Final_matrix_CT_1.xlsx"),
  overwrite = TRUE
)

tc_unizar_export <- matriz_a_tabla(
  tc_unizar$ids_originales,
  mat_unizar_final
)
write.xlsx(
  tc_unizar_export,
  file.path(output_dir, "Tecnical coefficients final.xlsx"),
  overwrite = TRUE
)

negativos_raw <- which(mat_unizar_raw < -tolerancia_rango, arr.ind = TRUE)
detalle_negativos <- if (nrow(negativos_raw) == 0) {
  tibble(
    Country = character(),
    Text = character(),
    Sector_destino = character(),
    Valor = numeric()
  )
} else {
  tibble(
    Country = tc_unizar$ids_originales$Country[negativos_raw[, "row"]],
    Text = tc_unizar$ids_originales$Text[negativos_raw[, "row"]],
    Sector_destino = colnames(mat_unizar_raw)[negativos_raw[, "col"]],
    Valor = mat_unizar_raw[negativos_raw]
  )
}
write.xlsx(
  list(
    RESUMEN = data.frame(
      Chequeo = "Coeficientes tecnicos intermedios negativos",
      Total = nrow(detalle_negativos),
      Check = if (nrow(detalle_negativos) == 0) "BIEN" else "REVISAR"
    ),
    DETALLE = detalle_negativos
  ),
  file.path(output_dir, "Valores_negativos.xlsx"),
  overwrite = TRUE
)

crear_libro_comprobaciones <- function(path) {
  hojas <- list(
    RESUMEN_CHECKS = resumen_checks,
    METODOLOGIA = metodologia,
    DISCREP_WILIAM_RAW = comparacion_raw$detalle,
    DISCREP_WILIAM_FINAL = comparacion_final_mrio$detalle,
    COLUMNAS_MEDIA_WILIAM = metodo_sobre_mrio$columnas,
    COLUMNAS_MEDIA_UNIZAR = tc_unizar_final$columnas,
    TC_WILIAM_RAW_CALC = matriz_a_tabla(
      tc_wiliam$ids_codigos,
      tc_wiliam$final
    ),
    TC_WILIAM_RAW_OFICIAL = matriz_a_tabla(
      ref_wiliam_raw$ids,
      ref_wiliam_raw$matriz
    ),
    TC_WILIAM_FINAL_CALC = matriz_a_tabla(
      tc_wiliam$ids_codigos,
      metodo_sobre_mrio$matriz
    ),
    TC_WILIAM_FINAL_OFICIAL = matriz_a_tabla(
      ref_wiliam_final$ids,
      ref_wiliam_final$matriz
    ),
    TC_UNIZAR_RAW = matriz_a_tabla(
      tc_unizar$ids_originales,
      mat_unizar_raw
    ),
    TC_UNIZAR_FINAL = tc_unizar_export
  )

  wb <- createWorkbook()
  header_style <- createStyle(
    fgFill = "#1F4E78",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )
  check_bien <- createStyle(fgFill = "#C6EFCE", fontColour = "#006100")
  check_revisar <- createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")
  check_doc <- createStyle(fgFill = "#FFEB9C", fontColour = "#9C6500")

  for (nombre in names(hojas)) {
    tabla <- as.data.frame(hojas[[nombre]])
    addWorksheet(wb, nombre)
    writeData(wb, nombre, tabla, withFilter = nrow(tabla) > 0)
    if (ncol(tabla) > 0) {
      addStyle(
        wb,
        nombre,
        header_style,
        rows = 1,
        cols = seq_len(ncol(tabla)),
        gridExpand = TRUE
      )
      freezePane(
        wb,
        nombre,
        firstActiveRow = 2,
        firstActiveCol = min(3, ncol(tabla) + 1)
      )
      setColWidths(
        wb,
        nombre,
        cols = seq_len(ncol(tabla)),
        widths = if (ncol(tabla) <= 8) "auto" else c(14, 34, rep(14, ncol(tabla) - 2))
      )
    }
  }

  if ("Check" %in% names(resumen_checks)) {
    col_check <- match("Check", names(resumen_checks))
    conditionalFormatting(
      wb,
      "RESUMEN_CHECKS",
      cols = col_check,
      rows = 2:(nrow(resumen_checks) + 1),
      rule = '=="BIEN"',
      style = check_bien
    )
    conditionalFormatting(
      wb,
      "RESUMEN_CHECKS",
      cols = col_check,
      rows = 2:(nrow(resumen_checks) + 1),
      rule = '=="REVISAR"',
      style = check_revisar
    )
    conditionalFormatting(
      wb,
      "RESUMEN_CHECKS",
      cols = col_check,
      rows = 2:(nrow(resumen_checks) + 1),
      rule = '=="DOCUMENTADO"',
      style = check_doc
    )
  }

  saveWorkbook(wb, path, overwrite = TRUE)
}

crear_libro_comprobaciones(
  file.path(
    checks_dir,
    "Comprobaciones coeficinete técnico.xlsx"
  )
)

print(resumen_checks)
