###################
# LIBRERIAS
###################
library(tidyverse)
library(readxl)
library(openxlsx)
library(writexl)

###################
# RUTAS Y PARAMETROS
###################

obtener_raiz_proyecto <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
    return(dirname(dirname(script_path)))
  }

  cwd <- normalizePath(".")
  if (dir.exists(file.path(cwd, "Base_Import_share_by_origin"))) {
    return(cwd)
  }
  if (basename(cwd) == "Base_Import_share_by_origin") {
    return(dirname(cwd))
  }

  stop("Ejecuta el script desde la raiz de TECNICAL_COEFFICIENT.")
}

project_root <- obtener_raiz_proyecto()
output_dir <- file.path(project_root, "Base_Import_share_by_origin")
data_dir <- file.path(project_root, "Data")

path_ref_local <- file.path(output_dir, "BISO_WILIAM_REFERENCIA.xlsx")
path_trade_wiliam <- file.path(
  dirname(project_root), "WILIAM", "model_parameters", "economy", "Trade.xlsx"
)
path_pp_to_bp_wiliam <- file.path(
  dirname(project_root), "WILIAM", "model_parameters", "economy", "PP_to_BP.xlsx"
)

tolerancia_comparacion <- 1e-10
tolerancia_redondeo_mrio <- 1e-4
tolerancia_suma_share <- 1e-9
tolerancia_rango <- 1e-12

###################
# CLASIFICACIONES
###################

pais_orden <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "UK",
  "China", "EASOC", "India", "LATAM", "Russia", "USMCA", "LROW"
)

sector_orden_original <- c(
  "CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL", "EXTRACTION_OIL",
  "EXTRACTION_GAS", "EXTRACTION_OTHER_GAS",
  "MINING_AND_MANUFACTURING_URANIUM_THORIUM",
  "MINING_AND_MANUFACTURING_IRON", "MINING_AND_MANUFACTURING_COPPER",
  "MINING_AND_MANUFACTURING_NICKEL", "MINING_AND_MANUFACTURING_ALUMINIUM",
  "MINING_AND_MANUFACTURING_PRECIOUS_METALS",
  "MINING_AND_MANUFACTURING_LEAD_ZINC_TIN",
  "MINING_AND_MANUFACTURING_OTHER_METALS", "MINING_NON_METALS",
  "MANUFACTURE_FOOD", "MANUFACTURE_WOOD", "COKE", "REFINING",
  "MANUFACTURE_CHEMICAL", "MANUFACTURE_PLASTIC",
  "MANUFACTURE_OTHER_NON_METAL", "HYDROGEN_PRODUCTION",
  "MANUFACTURE_METAL_PRODUCTS", "MANUFACTURE_ELECTRONICS",
  "MANUFACTURE_ELECTRICAL_EQUIPMENT", "MANUFACTURE_MACHINERY",
  "MANUFACTURE_VEHICLES", "MANUFACTURE_OTHER", "ELECTRICITY_COAL",
  "ELECTRICITY_GAS", "ELECTRICITY_NUCLEAR", "ELECTRICITY_HYDRO",
  "ELECTRICITY_WIND", "ELECTRICITY_OIL", "ELECTRICITY_SOLAR_PV",
  "ELECTRICITY_SOLAR_THERMAL", "ELECTRICITY_OTHER",
  "DISTRIBUTION_ELECTRICITY", "DISTRIBUTION_GAS", "STEAM_HOT_WATER",
  "WASTE_MANAGEMENT", "CONSTRUCTION", "TRADE_REPAIR_VEHICLES",
  "TRANSPORT_RAIL", "TRANSPORT_OTHER_LAND", "TRANSPORT_PIPELINE",
  "TRANSPORT_SEA", "TRANSPORT_INLAND_WATER", "TRANSPORT_AIR",
  "ACCOMMODATION", "TELECOMMUNICATIONS", "FINANCE", "REAL_ESTATE",
  "OTHER_SERVICES", "PUBLIC_ADMINISTRATION", "EDUCATION", "HEALTH",
  "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS"
)

final_demand_order <- c(
  "HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE",
  "NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS",
  "GENERAL_GOVERNMENT_FINAL_CONSUMPTION",
  "GROSS_FIXED_CAPITAL_FORMATION",
  "CHANGE_IN_INVENTORIES_AND_VALUABLES",
  "DIRECT_PURCHASES_ABROAD"
)

share_cols_order <- c(sector_orden_original, final_demand_order)
cols_id <- c("Pais", "Sector_Fila", "Pais_col")

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
  INDIA = "India",
  RUSSIA = "Russia",
  EASOC = "EASOC",
  LATAM = "LATAM",
  USMCA = "USMCA",
  LROW = "LROW"
)

###################
# FUNCIONES AUXILIARES
###################

map_to_wiliam_codes <- function(x) {
  out <- str_squish(as.character(x))
  idx <- out %in% names(map_country_to_wiliam)
  out[idx] <- unname(map_country_to_wiliam[out[idx]])
  out
}

normalizar_claves <- function(df) {
  df %>%
    mutate(
      Pais = map_to_wiliam_codes(Pais),
      Sector_Fila = str_squish(as.character(Sector_Fila)),
      Pais_col = map_to_wiliam_codes(Pais_col)
    )
}

ordenar_biso <- function(df) {
  df <- normalizar_claves(df)
  df$Pais <- factor(df$Pais, levels = pais_orden)
  df$Sector_Fila <- factor(df$Sector_Fila, levels = sector_orden_original)
  df$Pais_col <- factor(df$Pais_col, levels = pais_orden)

  df <- df %>% arrange(Pais, Sector_Fila, Pais_col)

  df$Pais <- as.character(df$Pais)
  df$Sector_Fila <- as.character(df$Sector_Fila)
  df$Pais_col <- as.character(df$Pais_col)
  df
}

clave_biso <- function(df) {
  do.call(paste, c(df[cols_id], sep = "\r"))
}

validar_claves <- function(df, nombre) {
  if (any(is.na(df[, cols_id]))) {
    stop(sprintf("%s contiene claves vacias.", nombre))
  }
  if (any(duplicated(df[, cols_id]))) {
    stop(sprintf("%s contiene claves duplicadas.", nombre))
  }
}

completar_columnas_share <- function(df) {
  faltantes <- setdiff(share_cols_order, names(df))
  if (length(faltantes) > 0) {
    df[faltantes] <- 0
  }
  df %>% select(all_of(cols_id), all_of(share_cols_order))
}

cargar_data_origin <- function(path_rdata) {
  env_tmp <- new.env(parent = emptyenv())
  load(path_rdata, envir = env_tmp)
  if (!exists("data_origin", envir = env_tmp)) {
    stop(sprintf("No se encontro el objeto data_origin en %s.", path_rdata))
  }
  get("data_origin", envir = env_tmp)
}

leer_tabla_cabecera_embebida <- function(path, sheet, key_names) {
  raw <- read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  if (nrow(raw) < 2 || ncol(raw) <= length(key_names)) {
    stop(sprintf("La hoja %s de %s no tiene la estructura esperada.", sheet, path))
  }

  header <- as.character(unlist(raw[1, ], use.names = FALSE))
  names(raw) <- c(key_names, header[(length(key_names) + 1):ncol(raw)])
  raw <- raw[-1, , drop = FALSE]
  as_tibble(raw)
}

normalizar_biso_referencia <- function(df) {
  names(df)[1:3] <- cols_id
  df <- normalizar_claves(df)

  share_cols <- intersect(share_cols_order, names(df))
  df <- df %>%
    filter(!is.na(Pais), !is.na(Sector_Fila), !is.na(Pais_col)) %>%
    mutate(across(all_of(share_cols), as.numeric))

  completar_columnas_share(df)
}

cargar_biso_wili_referencia <- function() {
  if (file.exists(path_trade_wiliam)) {
    df <- leer_tabla_cabecera_embebida(
      path_trade_wiliam,
      "EXO_Import_origin_shares",
      cols_id
    )
    return(normalizar_biso_referencia(df))
  }

  if (!file.exists(path_ref_local)) {
    stop(
      paste0(
        "No se encontro Trade.xlsx ni la referencia local ",
        path_ref_local, "."
      )
    )
  }

  sheets <- excel_sheets(path_ref_local)
  sheet_to_read <- if ("BISO_WILI" %in% sheets) "BISO_WILI" else sheets[[1]]
  normalizar_biso_referencia(read_excel(path_ref_local, sheet = sheet_to_read))
}

cargar_final_demand_pp_wiliam <- function(biso_wili_ref) {
  if (!file.exists(path_pp_to_bp_wiliam)) {
    return(biso_wili_ref %>% select(all_of(cols_id), all_of(final_demand_order)))
  }

  df <- leer_tabla_cabecera_embebida(
    path_pp_to_bp_wiliam,
    "BASE_Import_origin_shares_PP",
    cols_id
  )
  df <- normalizar_claves(df) %>%
    filter(!is.na(Pais), !is.na(Sector_Fila), !is.na(Pais_col)) %>%
    mutate(across(all_of(final_demand_order), as.numeric)) %>%
    select(all_of(cols_id), all_of(final_demand_order))

  validar_claves(df, "BASE_Import_origin_shares_PP")
  df
}

###################
# CALCULO BISO
###################

calcular_biso <- function(
    data_origin,
    tratamiento_negativos = c("conservar", "cero")) {
  tratamiento_negativos <- match.arg(tratamiento_negativos)

  data_bis <- data_origin %>%
    filter(
      !is.na(Pais),
      !is.na(Sector),
      Sector %in% sector_orden_original
    )

  value_cols <- setdiff(names(data_bis), c("Pais", "Sector"))
  base_names <- str_remove(value_cols, "\\d+$")
  pais_col <- str_extract(base_names, "^[^_]+")
  sector_col <- str_remove(base_names, "^[^_]+_")
  valid_countries <- names(map_country_to_wiliam)

  value_cols <- value_cols[
    pais_col %in% valid_countries &
      sector_col %in% share_cols_order
  ]

  numerador_biso_raw <- data_bis %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "Pais_columna",
      values_to = "Valor_original"
    ) %>%
    extract(
      Pais_columna,
      into = c("Pais_col", "Sector_col"),
      regex = "^([^_]+)_(.+)$"
    ) %>%
    mutate(
      Valor_original = as.numeric(Valor_original),
      Sector_limpio = str_remove(Sector_col, "\\d+$"),
      across(c(Pais, Sector, Pais_col), ~ str_squish(as.character(.))),
      Valor = if_else(
        tratamiento_negativos == "cero" & Valor_original < 0,
        0,
        Valor_original
      )
    ) %>%
    select(-Sector_col)

  valores_negativos <- numerador_biso_raw %>%
    filter(!is.na(Valor_original), Valor_original < 0) %>%
    transmute(
      Pais,
      Sector_Fila = Sector,
      Pais_col,
      Sector_Columna = Sector_limpio,
      Valor_original,
      Valor_usado = Valor
    )

  biso_long <- numerador_biso_raw %>%
    group_by(Pais, Sector, Pais_col, Sector_limpio) %>%
    summarise(
      numerador = sum(Valor, na.rm = TRUE),
      numerador_original = sum(Valor_original, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Pais_col, Sector, Sector_limpio) %>%
    mutate(
      denominador = sum(numerador[Pais != Pais_col], na.rm = TRUE),
      share = case_when(
        Pais == Pais_col ~ 0,
        is.na(denominador) ~ 0,
        abs(denominador) <= tolerancia_suma_share ~ 0,
        TRUE ~ numerador / denominador
      )
    ) %>%
    ungroup()

  biso_wide <- biso_long %>%
    select(
      Pais,
      Sector_Fila = Sector,
      Pais_col,
      Sector_limpio,
      share
    ) %>%
    pivot_wider(names_from = Sector_limpio, values_from = share) %>%
    completar_columnas_share() %>%
    mutate(across(all_of(share_cols_order), ~ replace_na(., 0))) %>%
    ordenar_biso()

  list(
    wide = biso_wide,
    long = biso_long,
    valores_negativos = valores_negativos
  )
}

check_suma_share <- function(biso_long, fuente) {
  biso_long %>%
    mutate(
      Pais_col = map_to_wiliam_codes(Pais_col),
      Sector_Fila = as.character(Sector),
      Sector_Columna = as.character(Sector_limpio)
    ) %>%
    group_by(Pais_col, Sector_Fila, Sector_Columna) %>%
    summarise(
      suma_share = sum(share, na.rm = TRUE),
      denominador = first(denominador),
      .groups = "drop"
    ) %>%
    mutate(
      esperado = if_else(
        is.na(denominador) | abs(denominador) <= tolerancia_suma_share,
        0,
        1
      ),
      error_abs = abs(suma_share - esperado),
      check_suma_share = if_else(
        error_abs <= tolerancia_suma_share,
        "BIEN",
        "REVISAR"
      ),
      fuente = fuente
    ) %>%
    select(
      fuente, Pais_col, Sector_Fila, Sector_Columna,
      suma_share, esperado, error_abs, check_suma_share
    )
}

detectar_fuera_rango <- function(df, fuente) {
  df %>%
    pivot_longer(
      cols = all_of(share_cols_order),
      names_to = "Sector_Columna",
      values_to = "share"
    ) %>%
    filter(
      !is.na(share),
      share < -tolerancia_rango | share > 1 + tolerancia_rango
    ) %>%
    mutate(
      fuente = fuente,
      desviacion_rango = pmax(-share, share - 1)
    ) %>%
    select(
      fuente, all_of(cols_id), Sector_Columna,
      share, desviacion_rango
    )
}

###################
# VALIDACION WILIAM
###################

validar_wiliam <- function(biso_calc, biso_ref, final_demand_pp) {
  validar_claves(biso_calc, "BISO calculada desde MRIO WILIAM")
  validar_claves(biso_ref, "BISO WILIAM de referencia")

  calc <- ordenar_biso(completar_columnas_share(biso_calc))
  ref <- ordenar_biso(completar_columnas_share(biso_ref))

  ref_idx <- match(clave_biso(calc), clave_biso(ref))
  if (anyNA(ref_idx) || nrow(calc) != nrow(ref)) {
    stop("Las claves de BISO calculada y BISO WILIAM no coinciden.")
  }
  ref <- ref[ref_idx, , drop = FALSE]

  pp <- ordenar_biso(final_demand_pp)
  pp_idx <- match(clave_biso(calc), clave_biso(pp))
  if (anyNA(pp_idx) || nrow(calc) != nrow(pp)) {
    stop("Las claves de demanda final PP no coinciden con BISO.")
  }
  pp <- pp[pp_idx, , drop = FALSE]

  diff_pp_ref <- abs(
    as.matrix(pp[, final_demand_order]) -
      as.matrix(ref[, final_demand_order])
  )
  if (max(diff_pp_ref, na.rm = TRUE) > tolerancia_comparacion) {
    stop("PP_to_BP y Trade.xlsx no coinciden en demanda final.")
  }

  discrepancias_intermedios <- list()

  for (sector_col in sector_orden_original) {
    diferencia <- abs(calc[[sector_col]] - ref[[sector_col]])
    idx_discrepancia <- which(diferencia > tolerancia_comparacion)
    if (length(idx_discrepancia) > 0) {
      discrepancias_intermedios[[sector_col]] <- tibble(
        Pais = calc$Pais[idx_discrepancia],
        Sector_Fila = calc$Sector_Fila[idx_discrepancia],
        Pais_col = calc$Pais_col[idx_discrepancia],
        Sector_Columna = sector_col,
        valor_mrio = calc[[sector_col]][idx_discrepancia],
        valor_wiliam = ref[[sector_col]][idx_discrepancia],
        diferencia_abs = diferencia[idx_discrepancia],
        fuera_tolerancia_1e_4 = if_else(
          diferencia[idx_discrepancia] > tolerancia_redondeo_mrio,
          "SI",
          "NO"
        ),
        observacion = paste0(
          "Diferencia entre Data_origin_WILIAM redondeada y ",
          "Trade.xlsx; no se sustituye el valor calculado"
        )
      )
    }
  }

  raw_final_diff <- abs(
    as.matrix(calc[, final_demand_order]) -
      as.matrix(ref[, final_demand_order])
  )

  discrepancias_intermedios <- if (length(discrepancias_intermedios) == 0) {
    tibble(
      Pais = character(),
      Sector_Fila = character(),
      Pais_col = character(),
      Sector_Columna = character(),
      valor_mrio = numeric(),
      valor_wiliam = numeric(),
      diferencia_abs = numeric(),
      fuera_tolerancia_1e_4 = character(),
      observacion = character()
    )
  } else {
    bind_rows(discrepancias_intermedios) %>%
      arrange(desc(diferencia_abs))
  }

  posiciones_final <- which(
    raw_final_diff > tolerancia_comparacion,
    arr.ind = TRUE
  )
  diferencias_final_top <- if (nrow(posiciones_final) == 0) {
    tibble(
      Pais = character(),
      Sector_Fila = character(),
      Pais_col = character(),
      Sector_Columna = character(),
      valor_mrio = numeric(),
      valor_oficial = numeric(),
      diferencia_abs = numeric()
    )
  } else {
    tibble(
      Pais = calc$Pais[posiciones_final[, "row"]],
      Sector_Fila = calc$Sector_Fila[posiciones_final[, "row"]],
      Pais_col = calc$Pais_col[posiciones_final[, "row"]],
      Sector_Columna = final_demand_order[posiciones_final[, "col"]],
      valor_mrio = as.matrix(calc[, final_demand_order])[posiciones_final],
      valor_oficial = as.matrix(ref[, final_demand_order])[posiciones_final],
      diferencia_abs = raw_final_diff[posiciones_final]
    ) %>%
      arrange(desc(diferencia_abs)) %>%
      slice_head(n = 1000)
  }

  n_diff_intermedios <- nrow(discrepancias_intermedios)
  n_fuera_intermedios <- sum(
    discrepancias_intermedios$diferencia_abs > tolerancia_redondeo_mrio
  )
  n_diff_final <- sum(raw_final_diff > tolerancia_comparacion, na.rm = TRUE)

  resumen_fuentes <- tibble(
    bloque = c("Intermedios (62)", "Demanda final (6)"),
    fuente_calculo = c(
      "Data_origin_WILIAM.RData",
      "Data_origin_WILIAM.RData (calculo MRIO crudo)"
    ),
    referencia = c(
      "Trade.xlsx / EXO_Import_origin_shares",
      "PP_to_BP.xlsx y Trade.xlsx (parametro oficial WILIAM)"
    ),
    celdas_diferentes = c(
      n_diff_intermedios,
      n_diff_final
    ),
    celdas_fuera_tolerancia = c(
      n_fuera_intermedios,
      NA_integer_
    ),
    max_diferencia_entrada = c(
      if_else(
        n_diff_intermedios == 0,
        0,
        max(discrepancias_intermedios$diferencia_abs)
      ),
      max(raw_final_diff, na.rm = TRUE)
    ),
    motivo = c(
      paste0(
        "La MRIO WILIAM esta redondeada. Se muestran las diferencias y ",
        "no se modifica el calculo."
      ),
      paste0(
        "La demanda final oficial procede de PP_to_BP a precios de ",
        "comprador; no es comparable directamente con la MRIO cruda."
      )
    ),
    check = c(
      if_else(n_fuera_intermedios == 0, "BIEN", "REVISAR"),
      "FUENTE_DISTINTA"
    )
  )

  list(
    biso_r = calc,
    referencia = ref,
    discrepancias_intermedios = discrepancias_intermedios,
    diferencias_final_top = diferencias_final_top,
    n_diff_intermedios = n_diff_intermedios,
    n_fuera_intermedios = n_fuera_intermedios,
    n_diff_final = n_diff_final,
    max_diff_final = max(raw_final_diff, na.rm = TRUE),
    resumen_fuentes = resumen_fuentes
  )
}

comparar_biso <- function(biso_r, biso_ref) {
  ref_idx <- match(clave_biso(biso_r), clave_biso(biso_ref))
  if (anyNA(ref_idx)) {
    stop("Faltan filas de referencia al comparar BISO_R.")
  }
  ref <- biso_ref[ref_idx, , drop = FALSE]

  diff_abs <- abs(
    as.matrix(biso_r[, share_cols_order]) -
      as.matrix(ref[, share_cols_order])
  )
  n_diferencias <- rowSums(diff_abs > tolerancia_comparacion)
  max_diferencia <- apply(diff_abs, 1, max)

  comparacion <- tibble(
    Pais = biso_r$Pais,
    Sector_Fila = biso_r$Sector_Fila,
    Pais_col = biso_r$Pais_col,
    fila_en_R = TRUE,
    fila_en_WILIAM = TRUE,
    n_celdas_diferentes_vs_WILIAM = n_diferencias,
    max_abs_diff_vs_WILIAM = max_diferencia,
    check_vs_WILIAM = if_else(n_diferencias == 0, "BIEN", "REVISAR")
  )

  list(
    filas = comparacion,
    total_diferencias = sum(diff_abs > tolerancia_comparacion),
    max_diferencia = max(diff_abs, na.rm = TRUE)
  )
}

###################
# EJECUCION
###################

data_origin_unizar <- cargar_data_origin(
  file.path(data_dir, "Data_origin_UNIZAR.RData")
)
data_origin_wiliam <- cargar_data_origin(
  file.path(data_dir, "Data_origin_WILIAM.RData")
)

# WILIAM conserva el tratamiento historico de negativos para poder reproducir
# exactamente los parametros oficiales existentes.
resultado_wiliam_mrio <- calcular_biso(
  data_origin_wiliam,
  tratamiento_negativos = "conservar"
)
biso_wili_ref <- ordenar_biso(cargar_biso_wili_referencia())
final_demand_pp <- cargar_final_demand_pp_wiliam(biso_wili_ref)

validacion_wiliam <- validar_wiliam(
  resultado_wiliam_mrio$wide,
  biso_wili_ref,
  final_demand_pp
)

biso_r <- ordenar_biso(validacion_wiliam$biso_r)
biso_wili_ref <- ordenar_biso(validacion_wiliam$referencia)
comparacion_wiliam <- comparar_biso(biso_r, biso_wili_ref)

biso_r_con_checks <- biso_r %>%
  left_join(
    comparacion_wiliam$filas %>%
      select(
        all_of(cols_id),
        fila_en_WILIAM,
        n_celdas_diferentes_vs_WILIAM,
        max_abs_diff_vs_WILIAM,
        check_vs_WILIAM
      ),
    by = cols_id
  )

# En UNIZAR se sustituyen flujos negativos por cero antes de normalizar.
# Asi el resultado es una share valida en [0, 1].
resultado_unizar <- calcular_biso(
  data_origin_unizar,
  tratamiento_negativos = "cero"
)
biso_unizar <- ordenar_biso(resultado_unizar$wide)
check_suma_unizar <- check_suma_share(resultado_unizar$long, "UNIZAR")
rango_unizar <- detectar_fuera_rango(biso_unizar, "UNIZAR")
rango_wiliam <- detectar_fuera_rango(biso_wili_ref, "WILIAM oficial")

resumen_suma_unizar_fila <- check_suma_unizar %>%
  group_by(Pais_col, Sector_Fila) %>%
  summarise(
    max_error_suma_share_UNIZAR = max(error_abs, na.rm = TRUE),
    check_suma_share_UNIZAR = if_else(
      all(check_suma_share == "BIEN"),
      "BIEN",
      "REVISAR"
    ),
    .groups = "drop"
  )

mat_unizar <- as.matrix(biso_unizar[, share_cols_order])
error_rango_fila <- apply(
  pmax(-mat_unizar, mat_unizar - 1, 0),
  1,
  max
)

biso_unizar_con_checks <- biso_unizar %>%
  left_join(
    resumen_suma_unizar_fila,
    by = c("Pais_col", "Sector_Fila")
  ) %>%
  mutate(
    max_error_rango_UNIZAR = error_rango_fila,
    check_rango_0_1_UNIZAR = if_else(
      max_error_rango_UNIZAR <= tolerancia_rango,
      "BIEN",
      "REVISAR"
    )
  )

ajustes_negativos_unizar <- resultado_unizar$valores_negativos %>%
  mutate(
    Pais = map_to_wiliam_codes(Pais),
    Pais_col = map_to_wiliam_codes(Pais_col),
    ajuste = Valor_usado - Valor_original,
    motivo = "Flujo negativo sustituido por 0 antes de normalizar"
  ) %>%
  arrange(Pais, Sector_Fila, Pais_col, Sector_Columna)

grupos_unizar_activos <- check_suma_unizar %>% filter(esperado == 1)
grupos_unizar_cero <- check_suma_unizar %>% filter(esperado == 0)
max_error_rango_unizar <- if (nrow(rango_unizar) == 0) {
  0
} else {
  max(rango_unizar$desviacion_rango)
}
max_error_rango_wiliam <- if (nrow(rango_wiliam) == 0) {
  0
} else {
  max(rango_wiliam$desviacion_rango)
}

resumen_checks <- tibble(
  chequeo = c(
    "BISO WILIAM intermedios: MRIO cruda vs referencia oficial",
    "BISO WILIAM demanda final: MRIO cruda vs fuente oficial",
    "Suma de shares UNIZAR en grupos con flujo",
    "Suma de shares UNIZAR en grupos sin flujo",
    "Rango [0,1] de BISO_UNIZAR",
    "Rango [0,1] de la referencia WILIAM"
  ),
  total = c(
    nrow(biso_r) * length(sector_orden_original),
    nrow(biso_r) * length(final_demand_order),
    nrow(grupos_unizar_activos),
    nrow(grupos_unizar_cero),
    nrow(biso_unizar) * length(share_cols_order),
    nrow(biso_wili_ref) * length(share_cols_order)
  ),
  bien = c(
    nrow(biso_r) * length(sector_orden_original) -
      validacion_wiliam$n_fuera_intermedios,
    nrow(biso_r) * length(final_demand_order) -
      validacion_wiliam$n_diff_final,
    sum(grupos_unizar_activos$check_suma_share == "BIEN"),
    sum(grupos_unizar_cero$check_suma_share == "BIEN"),
    nrow(biso_unizar) * length(share_cols_order) - nrow(rango_unizar),
    nrow(biso_wili_ref) * length(share_cols_order) - nrow(rango_wiliam)
  ),
  revisar = c(
    validacion_wiliam$n_fuera_intermedios,
    validacion_wiliam$n_diff_final,
    sum(grupos_unizar_activos$check_suma_share == "REVISAR"),
    sum(grupos_unizar_cero$check_suma_share == "REVISAR"),
    nrow(rango_unizar),
    nrow(rango_wiliam)
  ),
  max_error = c(
    if_else(
      nrow(validacion_wiliam$discrepancias_intermedios) == 0,
      0,
      max(validacion_wiliam$discrepancias_intermedios$diferencia_abs)
    ),
    validacion_wiliam$max_diff_final,
    max(grupos_unizar_activos$error_abs, na.rm = TRUE),
    max(grupos_unizar_cero$error_abs, na.rm = TRUE),
    max_error_rango_unizar,
    max_error_rango_wiliam
  ),
  check = c(
    if_else(
      validacion_wiliam$n_fuera_intermedios == 0,
      "BIEN",
      "REVISAR"
    ),
    "FUENTE_DISTINTA",
    if_else(all(grupos_unizar_activos$check_suma_share == "BIEN"), "BIEN", "REVISAR"),
    if_else(all(grupos_unizar_cero$check_suma_share == "BIEN"), "BIEN", "REVISAR"),
    if_else(nrow(rango_unizar) == 0, "BIEN", "REVISAR"),
    if_else(nrow(rango_wiliam) == 0, "BIEN", "REVISAR")
  )
)

resumen_forma_wiliam <- tibble(
  chequeo = c(
    "n_filas",
    "n_columnas",
    "claves_duplicadas_BISO_R",
    "claves_duplicadas_BISO_WILIAM",
    "celdas_diferentes_intermedios",
    "celdas_diferentes_demanda_final"
  ),
  valor_biso_r = c(
    nrow(biso_r),
    ncol(biso_r),
    sum(duplicated(biso_r[, cols_id])),
    NA_real_,
    validacion_wiliam$n_diff_intermedios,
    validacion_wiliam$n_diff_final
  ),
  valor_biso_wili = c(
    nrow(biso_wili_ref),
    ncol(biso_wili_ref),
    NA_real_,
    sum(duplicated(biso_wili_ref[, cols_id])),
    validacion_wiliam$n_diff_intermedios,
    validacion_wiliam$n_diff_final
  ),
  check = c(
    if_else(nrow(biso_r) == nrow(biso_wili_ref), "BIEN", "REVISAR"),
    if_else(ncol(biso_r) == ncol(biso_wili_ref), "BIEN", "REVISAR"),
    if_else(sum(duplicated(biso_r[, cols_id])) == 0, "BIEN", "REVISAR"),
    if_else(sum(duplicated(biso_wili_ref[, cols_id])) == 0, "BIEN", "REVISAR"),
    if_else(
      validacion_wiliam$n_fuera_intermedios == 0,
      "BIEN",
      "REVISAR"
    ),
    "FUENTE_DISTINTA"
  )
)

metodologia <- tibble(
  paso = 1:10,
  descripcion = c(
    "Intermedios WILIAM: calculados desde Data_origin_WILIAM.RData.",
    "La fuente MRIO WILIAM esta redondeada a 4 decimales.",
    "Las diferencias intermedias se muestran y se evaluan con valor absoluto.",
    "BISO_R conserva el valor calculado: no se sustituye por Trade.xlsx.",
    "Se usa 1e-4 como tolerancia de validacion de los intermedios WILIAM.",
    "La demanda final oficial WILIAM procede de PP_to_BP.xlsx a precios de comprador.",
    "La demanda final MRIO cruda se informa por separado porque usa una fuente distinta.",
    "UNIZAR: los flujos negativos se sustituyen por cero antes de normalizar.",
    "Los grupos UNIZAR con flujo deben sumar 1.",
    "Los grupos UNIZAR sin flujo deben sumar 0 y todas las shares quedar en [0,1]."
  )
)

###################
# EXPORTAR
###################

write_xlsx(
  as.data.frame(biso_unizar),
  file.path(output_dir, "BISO.xlsx")
)

write.xlsx(
  x = list(
    BISO_R = as.data.frame(biso_r_con_checks),
    BISO_WILI = as.data.frame(biso_wili_ref),
    COMPROBACION = as.data.frame(comparacion_wiliam$filas),
    BISO_UNIZAR = as.data.frame(biso_unizar_con_checks),
    CHECK_SUMA_SHARE_UNIZAR = as.data.frame(check_suma_unizar),
    RESUMEN_CHECKS = as.data.frame(resumen_checks),
    FUENTES_WILIAM = as.data.frame(validacion_wiliam$resumen_fuentes),
    DISCREPANCIAS_MRIO_WILIAM = as.data.frame(
      validacion_wiliam$discrepancias_intermedios
    ),
    DEMANDA_FINAL_DIF_TOP = as.data.frame(
      validacion_wiliam$diferencias_final_top
    ),
    AJUSTES_NEGATIVOS_UNIZAR = as.data.frame(ajustes_negativos_unizar),
    RANGO_WILIAM_OFICIAL = as.data.frame(rango_wiliam),
    RESUMEN_FORMA_WILIAM = as.data.frame(resumen_forma_wiliam),
    METODOLOGIA = as.data.frame(metodologia)
  ),
  file = file.path(output_dir, "Comprobaciones_BISO.xlsx"),
  overwrite = TRUE
)

print(resumen_checks)
print(validacion_wiliam$resumen_fuentes)
print(resumen_forma_wiliam)
