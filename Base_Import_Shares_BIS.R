install.packages(c("readxl", "tidyr", "dplyr", "openxlsx", "readr"))
install.packages("writexl")
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

sectores_columna <- c( "CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL", "EXTRACTION_OIL", 
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
                       "CHANGE_IN_INVENTORIES_AND_VALUABLES", "DIRECT_PURCHASES_ABROAD")
sectores_prioritarios<- c("CROPS", "ANIMALS", "FORESTRY", "FISHNG", "MINING_COAL", "EXTRACTION_OIL", 
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
                          "PUBLIC_ADMINISTRATION", "EDUCATION", "HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS")
sectores_finales <- c("HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE", "NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS", 
                      "GENERAL_GOVERNMENT_FINAL_CONSUMPTION", "GROSS_FIXED_CAPITAL_FORMATION", 
                      "CHANGE_IN_INVENTORIES_AND_VALUABLES", "DIRECT_PURCHASES_ABROAD")

Country<- c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECHREPUBLIC","DENMARK", 
                  "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
                  "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS", 
                  "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN", 
                  "UK", "CHINA", "EASOC", "INDIA", "LATAM", "RUSSIA", "USMCA", "LROW")
###################
#NUMERADOR 
###################

#Leer excel
 data_BIS_origin<-read.xlsx("./Base_Import_Share/DATA_BIS_ORIGIN.xlsx", colNames = TRUE) #lee el excel
 data_BIS_origin<-data_BIS_origin[1:2206,] #había datos que no se acababan de ir de calculos hechos en el excel, los he quitado
 data_BIS<-data_BIS_origin [!(data_BIS_origin[,2]%in%c("TAXES_LESS_SUBSIDIES_ON_PRODUCTS","VALUE_ADDED")),] #tambien quitamos ahora los taxes
 
# Convertirla en data.frame
  data_BIS <- as.data.frame(data_BIS)

# Obtener la lista de países y sectores de la matriz
  paises <- data_BIS[,1]  # Primera columna: nombres de países
  sectores<-data_BIS[,2] #Segunda columna:nombre de sectores

# Convertir la parte numérica en números (para evitar problemas de tipo)
  datos_numericos <- as.matrix(apply(data_BIS[, -c(1, 2)], 2, as.numeric))

# Revisar si hay valores NA después de la conversión
  datos_numericos[is.na(datos_numericos)] <- 0

# Crear la matriz donde se guardarán los resultados
  numerador_BIS <- matrix(0, nrow = nrow(data_BIS), ncol = ncol(data_BIS) - 2)
  columnas_totales <- colnames(data_BIS)[-c(1,2)]
# Iterar sobre cada fila
  for (fila in 1:nrow(data_BIS)) {
  
# Obtener el país y el sector de la fila actual
  pais_actual <- paises[fila]
  sector_actual <- sectores[fila]
  
# Encontrar las columnas que pertenecen al mismo país que la fila actual
  #columnas_mismo_pais <- which(grepl(paste0("^", pais_actual, "_")))
  columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
  
# Encontrar las filas que pertenecen al mismo sector (excluyendo la fila actual)
  filas_mismo_sector <- which(sectores == sector_actual)
  filas_a_sumar <- setdiff(filas_mismo_sector, fila)

# Si estamos en la diagonal principal (mismo país en fila y columna)
  if (length(columnas_mismo_pais) > 0 && length(filas_a_sumar) > 0) {
    
# Sumar los valores de la columna del mismo país y sector, excluyendo la fila actual
    suma_columnas <- colSums(as.matrix(datos_numericos[filas_a_sumar, columnas_mismo_pais, drop = FALSE]))
    
# Asignar la suma a la matriz de resultados
    numerador_BIS[fila, columnas_mismo_pais] <- suma_columnas
  }
}

# Convertir a dataframe
numerador_BIS_df <- as.data.frame(numerador_BIS)

# Agregar las columnas de países y sectores al data frame
numerador_BIS_df <- cbind(Pais = paises, Sector = sectores, numerador_BIS_df)

# Crear nombres únicos combinando País + Sector
#rownames(numerador_BIS_df) <- paste(paises, sectores, sep = "_")


# Crear nombres de columna para los sectores regulares (basados en las filas disponibles)
nombres_columnas_regulares <- paste(paises, sectores, sep = "_")

# Crear los países repetidos 6 veces (porque cada país debe aparecer 6 veces para los sectores finales)
paises_repetidos <- rep(unique(paises), each = 6)

# Crear nombres de columna para los sectores finales (usando los países repetidos)
nombres_columnas_finales <- paste(paises_repetidos, rep(sectores_finales, times = length(unique(paises))), sep = "_")

# Combinar ambos conjuntos de nombres
colnames(numerador_BIS_df) <- c("Pais", "Sector" ,nombres_columnas_regulares, nombres_columnas_finales)


# Crear nombres de columna asegurando que los sectores finales mantengan el país
#colnames(numerador_BIS_df) <- ifelse(sectores %in% sectores_finales, 
                                    # paste(paises, sectores_finales, sep = "_"), 
                                    # paste(paises, sectores, sep = "_"))

#orden_sectores <- match(sectores, sectores_prioritarios)
#orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1  # Los sectores no listados van al final

# Crear un vector lógico que indique si el sector está en la lista de sectores finales
#es_sector_final <- sectores %in% sectores_finales
# Convertir países y sectores en factores con el orden deseado

#paises_factor <- factor(paises, levels = Country, ordered = TRUE)
#sectores_factor <- factor(sectores, levels = orden_sectores, ordered = TRUE)

# Ordenar filas según los países y sectores dados
#orden_filas <- order(paises_factor, sectores_factor)

# Aplicar el orden a las filas del data frame
#numerador_BIS_df <- numerador_BIS_df[orden_filas, ]

# Ordenar columnas en el orden deseado
#orden_columnas <- match(colnames(numerador_BIS_df), lista_orden_columnas)

# Aplicar el orden a las columnas
#numerador_BIS_df <- numerador_BIS_df[, order(orden_columnas)]

# Crear un vector de prioridad para cada sector
#orden_sectores <- match(sectores, sectores_prioritarios)
#orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1  # Los sectores no listados van al final

# Crear un vector lógico que indique si el sector está en la lista de sectores finales
#es_sector_final <- sectores %in% sectores_finales

# Crear un vector de prioridad para cada país
#orden_paises <- match(paises, Country)  
#orden_paises[is.na(orden_paises)] <- length(Country) + 1  # Los países no listados van al final

# Ordenar por país según la prioridad dada, luego por sector y dejar los sectores finales al final
#orden_filas <- order(orden_paises, es_sector_final, orden_sectores)

# Aplicar el orden a la matriz
#numerador_BIS_df <- numerador_BIS_df[orden_filas, ]


# Ordenar primero por país, luego por la prioridad del sector y dejar los sectores finales al final
#orden_filas <- order(paises, es_sector_final, orden_sectores)
# Aplicar el orden a la matriz
#numerador_BIS_df <- numerador_BIS_df[orden_filas, ]


###################
#DENOMINADOR
###################

# Crear la matriz donde se guardarán los resultados
  denominador_BIS <- matrix(0, nrow = nrow(data_BIS), ncol = ncol(data_BIS) - 2)
  columnas_totales <- colnames(data_BIS)[-c(1,2)]
  
# Iterar sobre cada fila
  for (fila in 1:nrow(data_BIS)) {
    
# Obtener el país y el sector de la fila actual
    pais_actual <- paises[fila]
    sector_actual <- sectores[fila]
    
# Convertir la parte numérica en números (para evitar problemas de tipo)
    datos_numericos <- as.matrix(apply(data_BIS[, -c(1, 2)], 2, as.numeric))
    
# Encontrar las columnas que pertenecen al mismo país que la fila actual
# columnas_mismo_pais <- which(grepl(paste0("^", pais_actual, "_")))
    columnas_mismo_pais <- which(startsWith(columnas_totales, paste0(pais_actual, "_")))
    
# Encontrar las filas que pertenecen al mismo sector (excluyendo la fila actual)
    filas_mismo_sector <- which(sectores    == sector_actual)
    filas_a_sumar <- setdiff(filas_mismo_sector, fila)
    
# Si estamos en la diagonal principal (mismo país en fila y columna)
    if (length(columnas_mismo_pais) > 0 && length(filas_mismo_sector) > 0) {
      
# Sumar los valores de la columna del mismo país y sector, excluyendo la fila actual
      suma_columnas <- colSums(as.matrix(datos_numericos[filas_mismo_sector, columnas_mismo_pais, drop = FALSE]))
      
# Asignar la suma a la matriz de resultados
      denominador_BIS[fila, columnas_mismo_pais] <- suma_columnas
    }
  }

  # Convertir a dataframe
  denominador_BIS_df <- as.data.frame(denominador_BIS)
  
  # Agregar las columnas de países y sectores al data frame
  denominador_BIS_df <- cbind(Pais = paises, Sector = sectores, denominador_BIS_df)
  
  # Crear nombres únicos combinando País + Sector
  #rownames(denominador_BIS_df) <- paste(paises, sectores, sep = "_")
  
  
  # Crear nombres de columna para los sectores regulares (basados en las filas disponibles)
  nombres_columnas_regulares <- paste(paises, sectores, sep = "_")
  
  # Crear los países repetidos 6 veces (porque cada país debe aparecer 6 veces para los sectores finales)
  paises_repetidos <- rep(unique(paises), each = 6)
  
  # Crear nombres de columna para los sectores finales (usando los países repetidos)
  nombres_columnas_finales <- paste(paises_repetidos, rep(sectores_finales, times = length(unique(paises))), sep = "_")
  
  # Combinar ambos conjuntos de nombres
  colnames(denominador_BIS_df) <- c("Pais", "Sector" ,nombres_columnas_regulares, nombres_columnas_finales)
  
  
# Crear un vector de prioridad para cada sector
  orden_sectores <- match(sectores, sectores_prioritarios)
  orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1  # Los sectores no listados van al final
  
# Crear un vector lógico que indique si el sector está en la lista de sectores finales
  es_sector_final <- sectores %in% sectores_finales
 
   # Crear un vector de prioridad para cada país
  orden_paises <- match(paises, Country)  
  orden_paises[is.na(orden_paises)] <- length(Country) + 1  # Los países no listados van al final
  
  # Ordenar por país según la prioridad dada, luego por sector y dejar los sectores finales al final
  orden_filas <- order(orden_paises, es_sector_final, orden_sectores)
  
# Ordenar primero por país, luego por la prioridad del sector y dejar los sectores finales al final
  orden_filas <- order(paises, es_sector_final, orden_sectores)
  
  
# Aplicar el orden a la matriz
  denominador_BIS_df <- denominador_BIS_df[orden_filas, ]
#####################  
#IMPORT SHARE FINAL
#####################
intermediate_import_share_raw<- numerador_BIS_df %>% select(-1,-2)/denominador_BIS_df %>% select(-1,-2)
intermediate_import_share_raw[is.na(intermediate_import_share_raw)]<- 0
intermediate_import_share_raw<-cbind(denominador_BIS_df %>% select(1,2), intermediate_import_share_raw)
rownames(intermediate_import_share_raw) <- paste(paises, sectores, sep = "_")

#####################
#TRASPOSICIÓN
#################### 

# Cargar la matriz original
  intermediate_import_share_raw <- as.data.frame(intermediate_import_share_raw)


a <- intermediate_import_share_raw %>% 
  pivot_longer(cols = -c(-1,-2), 
               names_to = c("Country2", "Sector2"),    # Nombre para los países
               names_pattern = "([^_]*)_(.*)",) %>%  # cómo se ha pivotado, para pivotar solamente lo que esta en la primera barrabaja )

































# Obtener los nombres originales de las filas y columnas
  paises_sectores_filas <- rownames(intermediate_import_share_raw)
  paises_sectores_columnas <- colnames(intermediate_import_share_raw)
  
# Extraer país y sector de las filas
  paises_filas <- sub("_.*","",paises)  # Primer elemento -> País
  sectores_filas <- sub("^[^_]*_", "", sectores) # Segundo elemento -> Sector
  
# Segundo elemento -> Sector
  
# Extraer país y sector de las columnas
  paises_columnas <- sub("_.*","",paises_sectores_columnas)  # Primer elemento -> País
  sectores_columnas <- sub("^[^_]*_", "", paises_sectores_columnas) # Segundo elemento -> Sector
  
  
# Lista para almacenar las submatrices por país
  lista_matrices <- list()
  
  
  # Recorrer cada país
  for (pais in unique(paises_columnas)) {
    
# Seleccionar las filas y columnas donde el país coincide
    filas_pais <- which(paises_filas== pais)
    columnas_pais <- which(paises_columnas == pais)
    
# Extraer la submatriz cuadrada para ese país
    submatriz <- intermediate_import_share_raw[filas_pais, columnas_pais]
    
# Guardar en la lista
    lista_matrices[[pais]] <- submatriz
  }
  lista_matrices2 <- lapply(lista_matrices, function(df) {
    colnames(df) <- sub("^[^_]*_", "", colnames(df))
    return(df)
  })
  
  # Convertir la lista en una única matriz apilada
  resultado_matriz <- do.call(rbind, lista_matrices2)
  
  # Mostrar el resultado final
  #print(resultado_matriz)
  
# Definir los sectores prioritarios
  
# Crear una nueva matriz vacía con la misma cantidad de filas y solo las columnas de los sectores prioritarios
 # resultado_matriz <- matrix(0, nrow = length(paises_filas), ncol = length(sectores_columna))
  
# Iterar sobre cada fila para asignar valores solo a los sectores prioritarios
  #for (fila in 1:length(paises_filas)) {
    
# Obtener el país de la fila actual
    #pais_actual <- paises_filas[fila]
    
# Seleccionar solo las columnas del mismo país y sector que coincidan con sectores_prioritarios
    #columnas_sectores_validos <- which(paises_columnas == pais_actual)
    
# Asignar los valores correctos en la matriz resultante
    #resultado_matriz[fila, match(sectores_columnas[columnas_sectores_validos], sectores_columna)] <- 
     #as.numeric(intermediate_import_share_raw[fila, columnas_sectores_validos])
  #}
  
# Convertir la matriz a data.frame
  resultado_df <- as.data.frame(resultado_matriz)
  
# Agregar las dos primeras columnas con País y Sector
  #resultado_df <- cbind(País = paises_filas, Sector = sectores_filas, resultado_df)
  
# Asignar nombres de columnas (solo sectores)
  colnames(resultado_df) <- sectores_columna

  orden_sectores <- match(sectores, sectores_prioritarios)
  orden_sectores[is.na(orden_sectores)] <- length(sectores_prioritarios) + 1  # Los sectores no listados van al final
  
  # Crear un vector lógico que indique si el sector está en la lista de sectores finales
  es_sector_final <- sectores %in% sectores_finales
  
  # Ordenar primero por país, luego por la prioridad del sector y dejar los sectores finales al final
  orden_filas <- order(paises, es_sector_final, orden_sectores)
  
# Guardar el resultado en un archivo Excel (opcional)
  library(writexl)
  write_xlsx(resultado_df, "./Base_Import_Share/Base_Import_Share_R.xlsx")
  