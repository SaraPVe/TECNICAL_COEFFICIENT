library(openxlsx)

###################
# RUTAS
###################

obtener_raiz_proyecto <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }

  cwd <- normalizePath(getwd())
  if (file.exists(file.path(cwd, "Generar_outputs_generales.R"))) {
    return(cwd)
  }

  stop("Ejecuta este script desde la raiz de TECNICAL_COEFFICIENT.")
}

project_root <- obtener_raiz_proyecto()

checks_file <- file.path(
  project_root,
  "Comprobaciones_generales_TECHNICAL_COEFFICIENT.xlsx"
)
results_file <- file.path(
  project_root,
  "Resultados_WILIAM_TECHNICAL_COEFFICIENT.xlsx"
)

scripts_pipeline <- c(
  file.path(project_root, "Coeficientes_tecnicos", "Coeficientes técnicos.R"),
  file.path(project_root, "Base_Import_Share", "Base_Import_Shares_BIS.R"),
  file.path(
    project_root,
    "Base_Import_share_by_origin",
    "Base_Import_Share_by_Origin.R"
  )
)

partial_files <- list(
  tc_checks = file.path(
    project_root,
    "Coeficientes_tecnicos",
    "Comprobaciones",
    "Comprobaciones_TC.xlsx"
  ),
  tc_results = file.path(
    project_root,
    "Coeficientes_tecnicos",
    "Resultados_WILIAM_TC.xlsx"
  ),
  bis_checks = file.path(
    project_root,
    "Base_Import_Share",
    "Comprobaciones_base_import_share.xlsx"
  ),
  biso_checks = file.path(
    project_root,
    "Base_Import_share_by_origin",
    "Comprobaciones_BISO.xlsx"
  ),
  biso_results = file.path(
    project_root,
    "Base_Import_share_by_origin",
    "BISO.xlsx"
  )
)

intermediate_xlsx <- unique(c(
  unlist(partial_files, use.names = FALSE),
  file.path(project_root, "Comprobacion_integral_MRIO.xlsx"),
  file.path(project_root, "Base_Import_Share", "Base_Import_Share_R.xlsx"),
  file.path(
    project_root,
    "Base_Import_share_by_origin",
    "BISO_WILIAM_REFERENCIA.xlsx"
  ),
  file.path(
    project_root,
    "Base_Import_share_by_origin",
    "~$Comprobaciones_BISO.xlsx"
  ),
  file.path(project_root, "Coeficientes_tecnicos", "Final_matrix_CT_1.xlsx"),
  file.path(
    project_root,
    "Coeficientes_tecnicos",
    "Tecnical coefficients final.xlsx"
  ),
  file.path(project_root, "Coeficientes_tecnicos", "Valores_negativos.xlsx"),
  file.path(
    project_root,
    "Coeficientes_tecnicos",
    "Comprobaciones",
    "Comprobaciones coeficinete técnico.xlsx"
  )
))

###################
# CLASIFICACIONES
###################

pais_orden <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "UK",
  "China", "EASOC", "India", "LATAM", "Russia", "USMCA", "LROW"
)

sectores_intermedios <- c(
  "CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL",
  "EXTRACTION_OIL", "EXTRACTION_GAS", "EXTRACTION_OTHER_GAS",
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

demanda_final <- c(
  "HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE",
  "NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS",
  "GENERAL_GOVERNMENT_FINAL_CONSUMPTION",
  "GROSS_FIXED_CAPITAL_FORMATION",
  "CHANGE_IN_INVENTORIES_AND_VALUABLES",
  "DIRECT_PURCHASES_ABROAD"
)

map_wiliam_code_to_region_name <- c(
  AUT = "AUSTRIA",
  BEL = "BELGIUM",
  BGR = "BULGARIA",
  HRV = "CROATIA",
  CYP = "CYPRUS",
  CZE = "CZECH_REPUBLIC",
  DNK = "DENMARK",
  EST = "ESTONIA",
  FIN = "FINLAND",
  FRA = "FRANCE",
  DEU = "GERMANY",
  GRC = "GREECE",
  HUN = "HUNGARY",
  IRL = "IRELAND",
  ITA = "ITALY",
  LVA = "LATVIA",
  LTU = "LITHUANIA",
  LUX = "LUXEMBOURG",
  MLT = "MALTA",
  NLD = "NETHERLANDS",
  POL = "POLAND",
  PRT = "PORTUGAL",
  ROU = "ROMANIA",
  SVK = "SLOVAKIA",
  SVN = "SLOVENIA",
  ESP = "SPAIN",
  SWE = "SWEDEN",
  UK = "UK",
  China = "CHINA",
  EASOC = "EASOC",
  India = "INDIA",
  LATAM = "LATAM",
  Russia = "RUSSIA",
  USMCA = "USMCA",
  LROW = "LROW"
)

map_sector_to_range_name <- c(
  CROPS = "CROPS",
  ANIMALS = "ANIMALS",
  FORESTRY = "FORESTRY",
  FISHNG = "FISHING",
  MINING_COAL = "MINING_COAL",
  EXTRACTION_OIL = "EXTRACTION_OIL",
  EXTRACTION_GAS = "EXTRACTION_GAS",
  EXTRACTION_OTHER_GAS = "EXTRACTION_OTHER_GAS",
  MINING_AND_MANUFACTURING_URANIUM_THORIUM = "MINING_URANIUM_THORIUM",
  MINING_AND_MANUFACTURING_IRON = "MINING_IRON",
  MINING_AND_MANUFACTURING_COPPER = "MINING_COPPER",
  MINING_AND_MANUFACTURING_NICKEL = "MINING_NICKEL",
  MINING_AND_MANUFACTURING_ALUMINIUM = "MINING_ALUMINIUM",
  MINING_AND_MANUFACTURING_PRECIOUS_METALS = "MINING_PRECIOUS_METALS",
  MINING_AND_MANUFACTURING_LEAD_ZINC_TIN = "MINING_LEAD_ZINC_TIN",
  MINING_AND_MANUFACTURING_OTHER_METALS = "MINING_OTHER_METALS",
  MINING_NON_METALS = "MINING_NON_METALS",
  MANUFACTURE_FOOD = "MANUFACTURE_FOOD",
  MANUFACTURE_WOOD = "MANUFACTURE_WOOD",
  COKE = "COKE",
  REFINING = "REFINING",
  MANUFACTURE_CHEMICAL = "MANUFACTURE_CHEMICAL",
  MANUFACTURE_PLASTIC = "MANUFACTURE_PLASTIC",
  MANUFACTURE_OTHER_NON_METAL = "MANUFACTURE_OTHER_NON_METAL",
  HYDROGEN_PRODUCTION = "MANUFACTURE_BASIC_METALS",
  MANUFACTURE_METAL_PRODUCTS = "MANUFACTURE_METAL_PRODUCTS",
  MANUFACTURE_ELECTRONICS = "MANUFACTURE_ELECTRONICS",
  MANUFACTURE_ELECTRICAL_EQUIPMENT = "MANUFACTURE_ELECTRICAL_EQUIPMENT",
  MANUFACTURE_MACHINERY = "MANUFACTURE_MACHINERY",
  MANUFACTURE_VEHICLES = "MANUFACTURE_VEHICLES",
  MANUFACTURE_OTHER = "MANUFACTURE_OTHER",
  ELECTRICITY_COAL = "ELECTRICITY_COAL",
  ELECTRICITY_GAS = "ELECTRICITY_GAS",
  ELECTRICITY_NUCLEAR = "ELECTRICITY_NUCLEAR",
  ELECTRICITY_HYDRO = "ELECTRICITY_HYDRO",
  ELECTRICITY_WIND = "ELECTRICITY_WIND",
  ELECTRICITY_OIL = "ELECTRICITY_OIL",
  ELECTRICITY_SOLAR_PV = "ELECTRICITY_SOLAR_PV",
  ELECTRICITY_SOLAR_THERMAL = "ELECTRICITY_SOLAR_THERMAL",
  ELECTRICITY_OTHER = "ELECTRICITY_OTHER",
  DISTRIBUTION_ELECTRICITY = "DISTRIBUTION_ELECTRICITY",
  DISTRIBUTION_GAS = "DISTRIBUTION_GAS",
  STEAM_HOT_WATER = "STEAM_HOT_WATER",
  WASTE_MANAGEMENT = "WASTE_MANAGEMENT",
  CONSTRUCTION = "CONSTRUCTION",
  TRADE_REPAIR_VEHICLES = "TRADE_REPAIR_VEHICLES",
  TRANSPORT_RAIL = "TRANSPORT_RAIL",
  TRANSPORT_OTHER_LAND = "TRANSPORT_OTHER_LAND",
  TRANSPORT_PIPELINE = "TRANSPORT_PIPELINE",
  TRANSPORT_SEA = "TRANSPORT_SEA",
  TRANSPORT_INLAND_WATER = "TRANSPORT_INLAND_WATER",
  TRANSPORT_AIR = "TRANSPORT_AIR",
  ACCOMMODATION = "ACCOMMODATION",
  TELECOMMUNICATIONS = "TELECOMMUNICATIONS",
  FINANCE = "FINANCE",
  REAL_ESTATE = "REAL_ESTATE",
  OTHER_SERVICES = "OTHER_SERVICES",
  PUBLIC_ADMINISTRATION = "PUBLIC_ADMINISTRATION",
  EDUCATION = "EDUCATION",
  HEALTH = "HEALTH",
  ENTERTAIMENT = "ENTERTAIMENT",
  PRIVATE_HOUSEHOLDS = "PRIVATE_HOUSEHOLDS"
)

###################
# FUNCIONES
###################

assert_file <- function(path) {
  if (!file.exists(path)) {
    stop("No existe el fichero requerido: ", path)
  }
  invisible(path)
}

ejecutar_script <- function(path) {
  assert_file(path)
  cat("\n>>> Ejecutando ", path, "\n", sep = "")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(path)),
    stdout = "",
    stderr = ""
  )
  if (!identical(status, 0L)) {
    stop("Fallo al ejecutar ", path, " (codigo ", status, ").")
  }
}

leer_hoja <- function(path, sheet) {
  assert_file(path)
  if (!sheet %in% getSheetNames(path)) {
    stop("No existe la hoja ", sheet, " en ", path, ".")
  }
  read.xlsx(path, sheet = sheet)
}

crear_estilos <- function() {
  list(
    cabecera = createStyle(
      fontColour = "#FFFFFF",
      fgFill = "#1F4E78",
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE
    ),
    bien = createStyle(fgFill = "#C6EFCE", fontColour = "#006100"),
    revisar = createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006"),
    documentado = createStyle(fgFill = "#FFEB9C", fontColour = "#9C6500"),
    texto_largo = createStyle(wrapText = TRUE, valign = "top"),
    numero = createStyle(numFmt = "0.0000000000")
  )
}

ancho_columna <- function(nombre) {
  if (nombre %in% c("Chequeo", "chequeo", "Descripcion", "descripcion")) {
    return(60)
  }
  if (nombre %in% c("Explicacion", "explicacion", "Comentario", "comentario")) {
    return(85)
  }
  if (nombre %in% c("Pais", "Country", "Pais_col")) {
    return(14)
  }
  if (grepl("Sector|sector|Text", nombre)) {
    return(34)
  }
  if (nchar(nombre) > 18) {
    return(18)
  }
  max(12, nchar(nombre) + 2)
}

agregar_hoja_tabla <- function(wb, nombre, tabla, estilos, filtro = TRUE) {
  tabla <- as.data.frame(tabla)
  if (ncol(tabla) == 0) {
    tabla <- data.frame(Nota = "Sin registros")
  }

  addWorksheet(wb, nombre)
  writeData(wb, nombre, tabla, withFilter = filtro && nrow(tabla) > 0)

  addStyle(
    wb,
    nombre,
    estilos$cabecera,
    rows = 1,
    cols = seq_len(ncol(tabla)),
    gridExpand = TRUE
  )
  freezePane(wb, nombre, firstRow = TRUE, firstCol = TRUE)
  setRowHeights(wb, nombre, rows = 1, heights = 32)
  setColWidths(
    wb,
    nombre,
    cols = seq_len(ncol(tabla)),
    widths = vapply(names(tabla), ancho_columna, numeric(1))
  )

  estado_col <- intersect(
    c("Resultado", "Check", "check", "Estado", "estado"),
    names(tabla)
  )
  if (length(estado_col) > 0 && nrow(tabla) > 0) {
    col_estado <- match(estado_col[[1]], names(tabla))
    valores <- as.character(tabla[[estado_col[[1]]]])
    estilos_estado <- list(
      BIEN = estilos$bien,
      REVISAR = estilos$revisar,
      DOCUMENTADO = estilos$documentado,
      FUENTE_DISTINTA = estilos$documentado,
      DENTRO_TOLERANCIA = estilos$documentado
    )
    for (estado in names(estilos_estado)) {
      filas <- which(valores == estado) + 1
      if (length(filas) > 0) {
        addStyle(
          wb,
          nombre,
          estilos_estado[[estado]],
          rows = filas,
          cols = col_estado,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }

  texto_largo_cols <- which(
    names(tabla) %in%
      c("Explicacion", "explicacion", "Descripcion", "descripcion")
  )
  if (length(texto_largo_cols) > 0 && nrow(tabla) > 0) {
    addStyle(
      wb,
      nombre,
      estilos$texto_largo,
      rows = 2:(nrow(tabla) + 1),
      cols = texto_largo_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

normalizar_resumen <- function(df, bloque) {
  nombres <- tolower(names(df))
  buscar <- function(opciones) {
    pos <- match(opciones, nombres, nomatch = 0)
    pos <- pos[pos > 0]
    if (length(pos) == 0) {
      return(rep(NA, nrow(df)))
    }
    df[[pos[[1]]]]
  }

  total <- buscar(c("total", "total_celdas"))
  revisar <- buscar(c("revisar", "fuera_tolerancia"))
  bien <- buscar(c("bien"))
  if (all(is.na(bien)) && !all(is.na(total)) && !all(is.na(revisar))) {
    bien <- total - revisar
  }

  data.frame(
    Bloque = bloque,
    Chequeo = buscar(c("chequeo")),
    Total = total,
    Bien = bien,
    Revisar = revisar,
    Max_error = buscar(c("max_error", "max_diferencia_abs")),
    Resultado = buscar(c("resultado", "check")),
    stringsAsFactors = FALSE
  )
}

ordenar_por_pais_sector <- function(df, id_pais, id_sector) {
  df[[id_pais]] <- as.character(df[[id_pais]])
  df[[id_sector]] <- as.character(df[[id_sector]])
  df[[id_pais]] <- factor(df[[id_pais]], levels = pais_orden)
  df[[id_sector]] <- factor(df[[id_sector]], levels = sectores_intermedios)
  df <- df[order(df[[id_pais]], df[[id_sector]]), , drop = FALSE]
  df[[id_pais]] <- as.character(df[[id_pais]])
  df[[id_sector]] <- as.character(df[[id_sector]])
  row.names(df) <- NULL
  df
}

ordenar_biso <- function(df) {
  df$Pais <- factor(as.character(df$Pais), levels = pais_orden)
  df$Sector_Fila <- factor(
    as.character(df$Sector_Fila),
    levels = sectores_intermedios
  )
  df$Pais_col <- factor(as.character(df$Pais_col), levels = pais_orden)
  df <- df[order(df$Pais, df$Sector_Fila, df$Pais_col), , drop = FALSE]
  df$Pais <- as.character(df$Pais)
  df$Sector_Fila <- as.character(df$Sector_Fila)
  df$Pais_col <- as.character(df$Pais_col)
  row.names(df) <- NULL
  df
}

validar_resultados <- function(tc, bis, biso) {
  if (!all(c("Country", "Text", sectores_intermedios) %in% names(tc))) {
    stop("TC no contiene las columnas esperadas.")
  }
  if (!all(c("Pais", "Sector", sectores_intermedios) %in% names(bis))) {
    stop("BIS no contiene las columnas esperadas.")
  }
  if (!all(c("Pais", "Sector_Fila", "Pais_col", sectores_intermedios) %in%
    names(biso))) {
    stop("BISO no contiene las columnas esperadas.")
  }

  if (nrow(tc) != length(pais_orden) * length(sectores_intermedios)) {
    stop("TC no tiene 35 x 62 filas.")
  }
  if (nrow(bis) != length(pais_orden) * length(sectores_intermedios)) {
    stop("BIS no tiene 35 x 62 filas.")
  }
  if (nrow(biso) != length(pais_orden) * length(sectores_intermedios) *
    length(pais_orden)) {
    stop("BISO no tiene 35 x 62 x 35 filas.")
  }

  invisible(TRUE)
}

nombre_region <- function(codigo_pais) {
  out <- unname(map_wiliam_code_to_region_name[[codigo_pais]])
  if (is.null(out) || is.na(out)) {
    stop("No hay nombre WILIAM para el pais ", codigo_pais, ".")
  }
  out
}

nombre_sector_rango <- function(sector) {
  out <- unname(map_sector_to_range_name[[sector]])
  if (is.null(out) || is.na(out)) {
    stop("No hay nombre WILIAM para el sector ", sector, ".")
  }
  out
}

agregar_rangos_tc <- function(wb, sheet) {
  for (pais_idx in seq_along(pais_orden)) {
    fila_ini <- 2 + (pais_idx - 1) * length(sectores_intermedios)
    fila_fin <- fila_ini + length(sectores_intermedios) - 1
    createNamedRegion(
      wb,
      sheet = sheet,
      rows = fila_ini:fila_fin,
      cols = 3:(2 + length(sectores_intermedios)),
      name = paste0("A_MATRIX_TOTAL_", nombre_region(pais_orden[[pais_idx]]), "_UNIZ")
    )
  }
}

agregar_rangos_bis <- function(wb, sheet) {
  for (pais_idx in seq_along(pais_orden)) {
    fila_ini <- 2 + (pais_idx - 1) * length(sectores_intermedios)
    fila_fin <- fila_ini + length(sectores_intermedios) - 1
    createNamedRegion(
      wb,
      sheet = sheet,
      rows = fila_ini:fila_fin,
      cols = 3:(2 + length(sectores_intermedios)),
      name = paste0(
        "BASE_IMPORT_SHARES_INTERMEDIATES_",
        nombre_region(pais_orden[[pais_idx]]),
        "_UNIZ"
      )
    )
  }
}

agregar_rangos_biso <- function(wb, sheet) {
  for (pais_idx in seq_along(pais_orden)) {
    for (sector_idx in seq_along(sectores_intermedios)) {
      bloque_idx <- (pais_idx - 1) * length(sectores_intermedios) +
        (sector_idx - 1)
      fila_ini <- 2 + bloque_idx * length(pais_orden)
      fila_fin <- fila_ini + length(pais_orden) - 1
      createNamedRegion(
        wb,
        sheet = sheet,
        rows = fila_ini:fila_fin,
        cols = 4:(3 + length(sectores_intermedios)),
        name = paste0(
          "BASE_IMPORT_SHARES_BY_ORIGIN_BY_SECTORS_",
          nombre_region(pais_orden[[pais_idx]]),
          "_",
          nombre_sector_rango(sectores_intermedios[[sector_idx]]),
          "_UNIZ"
        )
      )
    }
  }
}

crear_libro_resultados <- function(tc, bis, biso, path) {
  estilos <- crear_estilos()
  wb <- createWorkbook()

  indice <- data.frame(
    Bloque = c("TC", "BIS", "BISO"),
    Hoja = c(
      "EXO_Technical_coefficients_UNIZ",
      "BASE_IMPORT_SHARES_UNIZ",
      "EXO_Import_origin_shares_UNIZ"
    ),
    Archivo_WILIAM = c("Production.xlsx", "Trade.xlsx", "Trade.xlsx"),
    Rango_WILIAM = c(
      "A_MATRIX_TOTAL_*_UNIZ",
      "BASE_IMPORT_SHARES_INTERMEDIATES_*_UNIZ",
      "BASE_IMPORT_SHARES_BY_ORIGIN_BY_SECTORS_*_*_UNIZ"
    ),
    Dimension_rango = c("62 x 62 por pais", "62 x 62 por pais", "35 x 62 por pais-sector"),
    Nota = c(
      "Copiar esta hoja a Production.xlsx o cambiar la ruta del modelo.",
      "Copiar esta hoja a Trade.xlsx o cambiar la ruta del modelo.",
      "Copiar esta hoja a Trade.xlsx o cambiar la ruta del modelo."
    ),
    stringsAsFactors = FALSE
  )

  agregar_hoja_tabla(wb, "INDICE", indice, estilos, filtro = FALSE)

  tc_export <- tc[, c("Country", "Text", sectores_intermedios), drop = FALSE]
  bis_export <- bis[, c("Pais", "Sector", sectores_intermedios, demanda_final), drop = FALSE]
  biso_export <- biso[
    ,
    c("Pais", "Sector_Fila", "Pais_col", sectores_intermedios, demanda_final),
    drop = FALSE
  ]

  agregar_hoja_tabla(
    wb,
    "EXO_Technical_coefficients_UNIZ",
    tc_export,
    estilos,
    filtro = FALSE
  )
  agregar_hoja_tabla(
    wb,
    "BASE_IMPORT_SHARES_UNIZ",
    bis_export,
    estilos,
    filtro = FALSE
  )
  agregar_hoja_tabla(
    wb,
    "EXO_Import_origin_shares_UNIZ",
    biso_export,
    estilos,
    filtro = FALSE
  )

  agregar_rangos_tc(wb, "EXO_Technical_coefficients_UNIZ")
  agregar_rangos_bis(wb, "BASE_IMPORT_SHARES_UNIZ")
  agregar_rangos_biso(wb, "EXO_Import_origin_shares_UNIZ")

  saveWorkbook(wb, path, overwrite = TRUE)
}

crear_libro_comprobaciones <- function(path) {
  estilos <- crear_estilos()

  tc_resumen <- leer_hoja(partial_files$tc_checks, "RESUMEN_CHECKS")
  bis_resumen <- leer_hoja(partial_files$bis_checks, "RESUMEN_CHECKS")
  biso_resumen <- leer_hoja(partial_files$biso_checks, "RESUMEN_CHECKS")

  resumen_general <- rbind(
    normalizar_resumen(tc_resumen, "TC"),
    normalizar_resumen(bis_resumen, "BIS"),
    normalizar_resumen(biso_resumen, "BISO")
  )

  metodologia_general <- data.frame(
    Paso = 1:8,
    Descripcion = c(
      "TC: coeficientes tecnicos calculados celda a celda; no se suman para validacion.",
      "TC UNIZAR: columnas pais-sector sin produccion mantenidas en cero.",
      "BIS: share importada agregada por pais-sector-destino-uso.",
      "BIS: se comprueba BIS importado + share domestica; grupos con flujo suman 1 y grupos sin flujo suman 0.",
      "BISO: share importada desagregada por pais de origen.",
      "BISO: la suma se hace sobre las filas de origen para cada pais destino, sector fila y uso.",
      "Los resultados WILIAM-ready se consolidan en Resultados_WILIAM_TECHNICAL_COEFFICIENT.xlsx.",
      "El libro general no reemplaza automaticamente Production.xlsx y Trade.xlsx: prepara hojas y rangos para copiar o referenciar."
    ),
    stringsAsFactors = FALSE
  )

  wb <- createWorkbook()
  agregar_hoja_tabla(wb, "RESUMEN_GENERAL", resumen_general, estilos)
  agregar_hoja_tabla(wb, "METODOLOGIA_GENERAL", metodologia_general, estilos)

  hojas <- list(
    TC_RESUMEN = tc_resumen,
    TC_COLUMNAS_CERO = leer_hoja(partial_files$tc_checks, "COLUMNAS_CERO_UNIZAR"),
    TC_FUERA_RANGO = leer_hoja(partial_files$tc_checks, "TC_UNIZAR_FUERA_RANGO"),
    BIS_RESUMEN = bis_resumen,
    BIS_CHECK_SUMA_UNIZAR = leer_hoja(
      partial_files$bis_checks,
      "CHECK_SUMA_SHARE_UNIZAR"
    ),
    BIS_FUERA_RANGO_RAW = leer_hoja(
      partial_files$bis_checks,
      "FUERA_RANGO_UNIZAR_RAW"
    ),
    BIS_NEGATIVOS_UNIZAR = leer_hoja(
      partial_files$bis_checks,
      "AJUSTES_NEGATIVOS_UNIZAR"
    ),
    BISO_RESUMEN = biso_resumen,
    BISO_FORMA_WILIAM = leer_hoja(
      partial_files$biso_checks,
      "RESUMEN_FORMA_WILIAM"
    ),
    BISO_CHECK_SUMA_UNIZAR = leer_hoja(
      partial_files$biso_checks,
      "CHECK_SUMA_SHARE_UNIZAR"
    ),
    BISO_RANGO_WILIAM = leer_hoja(
      partial_files$biso_checks,
      "RANGO_WILIAM_OFICIAL"
    ),
    BISO_NEGATIVOS_UNIZAR = leer_hoja(
      partial_files$biso_checks,
      "AJUSTES_NEGATIVOS_UNIZAR"
    )
  )

  for (nombre in names(hojas)) {
    agregar_hoja_tabla(wb, nombre, hojas[[nombre]], estilos)
  }

  saveWorkbook(wb, path, overwrite = TRUE)
}

limpiar_excels_intermedios <- function(paths) {
  if (identical(Sys.getenv("TC_KEEP_INTERMEDIATE_XLSX"), "1")) {
    cat("\nSe conservan los Excel intermedios por TC_KEEP_INTERMEDIATE_XLSX=1.\n")
    return(invisible(NULL))
  }

  paths <- normalizePath(paths[file.exists(paths)], mustWork = FALSE)
  paths <- setdiff(paths, normalizePath(c(checks_file, results_file), mustWork = FALSE))

  if (length(paths) == 0) {
    cat("\nNo hay Excel intermedios para limpiar.\n")
    return(invisible(NULL))
  }

  borrados <- file.remove(paths)
  if (!all(borrados)) {
    stop(
      "No se pudieron borrar estos Excel intermedios: ",
      paste(paths[!borrados], collapse = ", ")
    )
  }

  cat("\nExcel intermedios eliminados:\n")
  cat(paste0("- ", paths, collapse = "\n"), "\n", sep = "")
  invisible(paths)
}

###################
# EJECUCION
###################

oldwd <- setwd(project_root)
on.exit(setwd(oldwd), add = TRUE)

if (!identical(Sys.getenv("TC_SKIP_SUBSCRIPTS"), "1")) {
  for (script in scripts_pipeline) {
    ejecutar_script(script)
  }
}

for (path in partial_files) {
  assert_file(path)
}

tc <- leer_hoja(partial_files$tc_results, "TC_UNIZAR_FINAL")
bis <- leer_hoja(partial_files$bis_checks, "BIS_UNIZAR_AJUSTADO")
biso <- leer_hoja(partial_files$biso_results, "Sheet1")

tc <- ordenar_por_pais_sector(tc, "Country", "Text")
bis <- ordenar_por_pais_sector(bis, "Pais", "Sector")
biso <- ordenar_biso(biso)

validar_resultados(tc, bis, biso)

crear_libro_comprobaciones(checks_file)
crear_libro_resultados(tc, bis, biso, results_file)
limpiar_excels_intermedios(intermediate_xlsx)

cat("\nComprobaciones generales: ", checks_file, "\n", sep = "")
cat("Resultados WILIAM generales: ", results_file, "\n", sep = "")
