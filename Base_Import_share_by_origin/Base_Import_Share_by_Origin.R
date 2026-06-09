###################
# LIBRERIAS
###################
library(tidyverse)   # includes dplyr, tidyr, stringr, readr, etc.
library(readxl)
library(openxlsx)
library(writexl)

###################
# PARAMETROS DE COMPROBACION
###################
tolerancia_comparacion <- 1e-6
tolerancia_suma_share  <- 1e-6
path_ref_wiliam <- "./Base_Import_share_by_origin/BISO_WILIAM_REFERENCIA.xlsx"

pais_orden <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "UK",+
  "China", "EASOC", "India", "LATAM", "Russia", "USMCA", "LROW"
)

###################
# 62 sectores validos
###################
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

###################
# FUNCIONES AUXILIARES
###################

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

normalizar_hoja_biso <- function(df) {
  if (ncol(df) < 3) {
    stop("La hoja de referencia debe tener al menos 3 columnas clave.")
  }

  names(df)[1:3] <- c("Pais", "Sector_Fila", "Pais_col")

  df %>%
    mutate(
      across(
        all_of(c("Pais", "Sector_Fila", "Pais_col")),
        ~ str_squish(as.character(.))
      )
    )
}

map_to_wiliam_codes <- function(x) {
  out <- as.character(x)
  idx <- out %in% names(map_country_to_wiliam)
  out[idx] <- unname(map_country_to_wiliam[out[idx]])
  out
}

adaptar_formato_pais_calc <- function(calc_df, ref_df) {
  ref_codes <- unname(map_country_to_wiliam)
  ref_names <- names(map_country_to_wiliam)

  score_code <- mean(ref_df$Pais %in% ref_codes, na.rm = TRUE) +
    mean(ref_df$Pais_col %in% ref_codes, na.rm = TRUE)
  score_name <- mean(ref_df$Pais %in% ref_names, na.rm = TRUE) +
    mean(ref_df$Pais_col %in% ref_names, na.rm = TRUE)

  if (score_code > score_name) {
    calc_df %>%
      mutate(
        Pais = map_to_wiliam_codes(Pais),
        Pais_col = map_to_wiliam_codes(Pais_col)
      )
  } else {
    calc_df
  }
}

cargar_biso_wili_referencia <- function(path_ref_wiliam) {
  if (!file.exists(path_ref_wiliam)) {
    stop(
      paste0(
        "No se encontro la referencia WILIAM en ",
        path_ref_wiliam,
        ". Guarda tu BISO de WILIAM en ese archivo (hoja BISO_WILI)."
      )
    )
  }

  sheets <- excel_sheets(path_ref_wiliam)
  sheet_to_read <- if ("BISO_WILI" %in% sheets) "BISO_WILI" else sheets[[1]]
  normalizar_hoja_biso(read_excel(path_ref_wiliam, sheet = sheet_to_read))
}

cargar_data_origin <- function(path_rdata) {
  env_tmp <- new.env(parent = emptyenv())
  load(path_rdata, envir = env_tmp)
  if (!exists("data_origin", envir = env_tmp)) {
    stop(sprintf("No se encontro el objeto 'data_origin' en %s", path_rdata))
  }
  get("data_origin", envir = env_tmp)
}

completar_columnas_share <- function(df, id_cols, share_cols_objetivo) {
  cols_share_actual <- setdiff(names(df), id_cols)
  cols_faltantes <- setdiff(share_cols_objetivo, cols_share_actual)

  if (length(cols_faltantes) > 0) {
    df[cols_faltantes] <- 0
  }

  df %>%
    select(all_of(id_cols), all_of(share_cols_objetivo))
}

calcular_biso <- function(data_origin, sector_orden_original, tolerancia_suma_share) {
  data_bis <- data_origin[1:2206, ]
  data_bis <- data_bis[data_bis$Sector %in% sector_orden_original, ]

  numerador_biso_raw <- data_bis %>%
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
      Valor = as.numeric(Valor),
      across(c(Pais, Sector, Pais_col), ~ str_squish(as.character(.))),
      Sector_limpio = str_remove(Sector_col, "\\d+$")
    ) %>%
    select(-Sector_col) %>%
    filter(Sector %in% sector_orden_original)

  biso_long <- numerador_biso_raw %>%
    group_by(Pais, Sector, Pais_col, Sector_limpio) %>%
    summarise(
      numerador = sum(Valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Pais_col, Sector, Sector_limpio) %>%
    mutate(
      denominador = sum(numerador[Pais != Pais_col], na.rm = TRUE),
      share = case_when(
        Pais == Pais_col                              ~ 0,
        is.na(denominador)                            ~ 0,
        abs(denominador) <= tolerancia_suma_share     ~ 0,
        TRUE                                          ~ numerador / denominador
      )
    ) %>%
    ungroup()

  biso_wide <- biso_long %>%
    select(Pais, Sector_Fila = Sector, Pais_col, Sector_limpio, share) %>%
    pivot_wider(names_from = Sector_limpio, values_from = share)

  cols_id <- c("Pais", "Sector_Fila", "Pais_col")
  cols_sector_actuales <- setdiff(names(biso_wide), cols_id)

  cols_sector_ordenadas <- intersect(sector_orden_original, cols_sector_actuales)
  cols_sector_extras    <- setdiff(cols_sector_actuales, cols_sector_ordenadas)
  cols_sector_final     <- c(cols_sector_ordenadas, sort(cols_sector_extras))

  biso_wide <- biso_wide %>%
    select(all_of(cols_id), all_of(cols_sector_final)) %>%
    mutate(
      Sector_Fila = factor(as.character(Sector_Fila), levels = sector_orden_original),
      across(where(is.numeric), ~ replace_na(., 0))
    ) %>%
    arrange(Pais, Sector_Fila, Pais_col) %>%
    mutate(Sector_Fila = as.character(Sector_Fila))

  list(wide = biso_wide, long = biso_long)
}

check_suma_share <- function(biso_long, fuente, tolerancia_suma_share) {
  biso_long %>%
    mutate(
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
      esperado = if_else(is.na(denominador) | abs(denominador) <= tolerancia_suma_share, 0, 1),
      error_abs = abs(suma_share - esperado),
      check_suma_share = if_else(error_abs <= tolerancia_suma_share, "BIEN", "REVISAR"),
      fuente = fuente
    ) %>%
    select(fuente, Pais_col, Sector_Fila, Sector_Columna,
           suma_share, esperado, error_abs, check_suma_share)
}

ordenar_biso <- function(df, pais_ord = pais_orden, sector_ord = sector_orden_original) {
  for (col in intersect(c("Pais", "Pais_col"), names(df))) {
    df[[col]] <- factor(df[[col]], levels = pais_ord)
  }
  if ("Sector_Fila" %in% names(df)) {
    df[["Sector_Fila"]] <- factor(df[["Sector_Fila"]], levels = sector_ord)
  }
  sort_cols <- intersect(c("Pais", "Sector_Fila", "Pais_col"), names(df))
  df <- df %>% arrange(across(all_of(sort_cols)))
  for (col in intersect(c("Pais", "Pais_col", "Sector_Fila"), names(df))) {
    df[[col]] <- as.character(df[[col]])
  }
  df
}

###################
# CALCULO BISO (UNIZAR y WILIAM)
###################
data_origin_unizar <- cargar_data_origin("Data/Data_origin_UNIZAR.RData")
data_origin_wiliam <- cargar_data_origin("Data/Data_origin_WILIAM.RData")

resultado_unizar <- calcular_biso(data_origin_unizar, sector_orden_original, tolerancia_suma_share)
resultado_wiliam_calc <- calcular_biso(data_origin_wiliam, sector_orden_original, tolerancia_suma_share)

biso_unizar <- resultado_unizar$wide
biso_r <- resultado_wiliam_calc$wide  # BISO calculada en R a partir de Data_origin_WILIAM
biso_wili_ref <- cargar_biso_wili_referencia(path_ref_wiliam)

biso_r <- adaptar_formato_pais_calc(biso_r, biso_wili_ref)

cols_id <- c("Pais", "Sector_Fila", "Pais_col")
share_cols_union <- union(
  setdiff(names(biso_r), cols_id),
  setdiff(names(biso_wili_ref), cols_id)
)

biso_r <- completar_columnas_share(biso_r, cols_id, share_cols_union)
biso_wili_ref <- completar_columnas_share(biso_wili_ref, cols_id, share_cols_union)

if (any(duplicated(biso_r[, cols_id]))) {
  stop("Hay claves duplicadas en BISO_R (Pais_col, Sector_Fila, Pais).")
}

if (any(duplicated(biso_wili_ref[, cols_id]))) {
  stop("Hay claves duplicadas en BISO_WILI de referencia (Pais_col, Sector_Fila, Pais).")
}

###################
# COMPROBACION 1: BISO_R (desde Data_origin_WILIAM) vs BISO_WILI (referencia)
###################
biso_r_cmp <- biso_r %>%
  mutate(fila_en_R = TRUE) %>%
  rename_with(~ paste0(.x, "_R"), all_of(share_cols_union))

biso_w_cmp <- biso_wili_ref %>%
  mutate(fila_en_WILIAM = TRUE) %>%
  rename_with(~ paste0(.x, "_WILIAM"), all_of(share_cols_union))

comparacion_base <- full_join(biso_r_cmp, biso_w_cmp, by = cols_id) %>%
  mutate(
    fila_en_R = replace_na(fila_en_R, FALSE),
    fila_en_WILIAM = replace_na(fila_en_WILIAM, FALSE)
  )

cols_r <- paste0(share_cols_union, "_R")
cols_w <- paste0(share_cols_union, "_WILIAM")

mat_r <- as.matrix(comparacion_base[, cols_r, drop = FALSE])
mat_w <- as.matrix(comparacion_base[, cols_w, drop = FALSE])

diff_abs <- abs(replace(mat_r, is.na(mat_r), 0) - replace(mat_w, is.na(mat_w), 0))
diff_abs[is.na(mat_r) | is.na(mat_w)] <- Inf

n_diferencias_fila <- rowSums(diff_abs > tolerancia_comparacion)
max_diff_fila <- apply(diff_abs, 1, max)

check_vs_wiliam_fila <- comparacion_base %>%
  transmute(
    Pais,
    Sector_Fila,
    Pais_col,
    fila_encontrada_en_WILIAM = fila_en_WILIAM,
    n_celdas_diferentes_vs_WILIAM = n_diferencias_fila,
    max_abs_diff_vs_WILIAM = if_else(is.infinite(max_diff_fila), NA_real_, max_diff_fila),
    check_vs_WILIAM = if_else(fila_en_WILIAM & n_diferencias_fila == 0, "BIEN", "REVISAR")
  )

biso_r_con_checks <- biso_r %>%
  left_join(check_vs_wiliam_fila, by = cols_id)

comprobacion_resumen <- comparacion_base %>%
  transmute(
    Pais,
    Sector_Fila,
    Pais_col,
    fila_en_R,
    fila_en_WILIAM,
    n_celdas_diferentes_vs_WILIAM = n_diferencias_fila,
    max_abs_diff_vs_WILIAM = if_else(is.infinite(max_diff_fila), NA_real_, max_diff_fila),
    check_vs_WILIAM = if_else(fila_en_R & fila_en_WILIAM & n_diferencias_fila == 0, "BIEN", "REVISAR")
  )

n_ref_sin_calc <- sum(!comparacion_base$fila_en_R & comparacion_base$fila_en_WILIAM)
n_calc_sin_ref <- sum(comparacion_base$fila_en_R & !comparacion_base$fila_en_WILIAM)

resumen_forma_wiliam <- tibble(
  chequeo = c(
    "n_filas",
    "n_columnas",
    "filas_referencia_sin_calculo",
    "filas_calculo_sin_referencia"
  ),
  valor_biso_r = c(
    nrow(biso_r),
    ncol(biso_r),
    n_ref_sin_calc,
    n_calc_sin_ref
  ),
  valor_biso_wili = c(
    nrow(biso_wili_ref),
    ncol(biso_wili_ref),
    n_ref_sin_calc,
    n_calc_sin_ref
  ),
  check = c(
    if_else(nrow(biso_r) == nrow(biso_wili_ref), "BIEN", "REVISAR"),
    if_else(ncol(biso_r) == ncol(biso_wili_ref), "BIEN", "REVISAR"),
    if_else(n_ref_sin_calc == 0, "BIEN", "REVISAR"),
    if_else(n_calc_sin_ref == 0, "BIEN", "REVISAR")
  )
)

###################
# COMPROBACION 2: UNIZAR (suma de shares = 1)
###################
check_suma_unizar <- check_suma_share(resultado_unizar$long, "UNIZAR", tolerancia_suma_share)

resumen_suma_unizar_fila <- check_suma_unizar %>%
  group_by(Pais_col, Sector_Fila) %>%
  summarise(
    max_error_suma_share_UNIZAR = max(error_abs, na.rm = TRUE),
    check_suma_share_UNIZAR = if_else(all(check_suma_share == "BIEN"), "BIEN", "REVISAR"),
    .groups = "drop"
  )

biso_unizar_con_checks <- biso_unizar %>%
  left_join(resumen_suma_unizar_fila, by = c("Pais_col", "Sector_Fila"))

resumen_checks <- bind_rows(
  tibble(
    chequeo = "Comparacion BISO_R vs BISO_WILIAM (por fila)",
    total = nrow(biso_r_con_checks),
    bien = sum(biso_r_con_checks$check_vs_WILIAM == "BIEN", na.rm = TRUE),
    revisar = sum(biso_r_con_checks$check_vs_WILIAM == "REVISAR", na.rm = TRUE),
    max_error = if_else(
      all(is.na(biso_r_con_checks$max_abs_diff_vs_WILIAM)),
      NA_real_,
      max(biso_r_con_checks$max_abs_diff_vs_WILIAM, na.rm = TRUE)
    )
  ),
  tibble(
    chequeo = "Suma de shares = 1 (UNIZAR)",
    total = nrow(check_suma_unizar),
    bien = sum(check_suma_unizar$check_suma_share == "BIEN", na.rm = TRUE),
    revisar = sum(check_suma_unizar$check_suma_share == "REVISAR", na.rm = TRUE),
    max_error = max(check_suma_unizar$error_abs, na.rm = TRUE)
  )
)

###################
# ORDENAR OUTPUTS POR PAIS
###################
biso_unizar            <- ordenar_biso(biso_unizar)
biso_r_con_checks      <- ordenar_biso(biso_r_con_checks)
biso_wili_ref          <- ordenar_biso(biso_wili_ref)
biso_unizar_con_checks <- ordenar_biso(biso_unizar_con_checks)
comprobacion_resumen   <- ordenar_biso(comprobacion_resumen)
check_suma_unizar      <- ordenar_biso(check_suma_unizar)   # ordena por Pais_col + Sector_Fila

###################
# EXPORTAR
###################
write_xlsx(biso_unizar, "./Base_Import_share_by_origin/BISO.xlsx")

write.xlsx(
  x = list(
    BISO_R = as.data.frame(biso_r_con_checks),
    BISO_WILI = as.data.frame(biso_wili_ref),
    COMPROBACION = as.data.frame(comprobacion_resumen),
    BISO_UNIZAR = as.data.frame(biso_unizar_con_checks),
    CHECK_SUMA_SHARE_UNIZAR = as.data.frame(check_suma_unizar),
    RESUMEN_CHECKS = as.data.frame(resumen_checks),
    RESUMEN_FORMA_WILIAM = as.data.frame(resumen_forma_wiliam)
  ),
  file = "./Base_Import_share_by_origin/Comprobaciones_BISO.xlsx",
  overwrite = TRUE
)

print(resumen_checks)
print(resumen_forma_wiliam)