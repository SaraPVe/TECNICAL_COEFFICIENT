# install.packages(c("readxl", "tidyr", "dplyr", "openxlsx", "readr"))
# install.packages("writexl")
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


#########
#Numerador
#########
data_BIS_origin<-read.xlsx("DATA_BIS_ORIGIN.xlsx", colNames = TRUE) #lee el excel
data_BIS_origin<-data_BIS_origin[1:2206,] #había datos que no se acababan de ir de calculos hechos en el excel, los he quitado
data_BIS<-data_BIS_origin [!(data_BIS_origin[,2]%in%c("TAXES_LESS_SUBSIDIES_ON_PRODUCTS","VALUE_ADDED")),]

  Numerador_BISO_raw <- data_BIS %>%
    pivot_longer(cols = -c(Pais, Sector),  # Pivotar todas las columnas excepto PAIS y SECTOR
                 names_to = "Pais_columna",  
                 values_to = "Valor") %>%  
  relocate(Pais_columna, .after = Sector) %>%
  group_by(Pais, Pais_columna) %>% 
  separate(col = Pais_columna, into = c("Pais_col", "Sector_col"), sep = "_", extra = "merge") %>%
  mutate(Sector_limpio=str_remove(Sector_col,"\\d+$"))
  
  Numerador_BISO_raw$Sector_col<-NULL
  
  Numerador_BISO<-Numerador_BISO_raw %>%  
  pivot_wider(names_from = Sector_limpio, values_from = Valor) %>% 
  arrange(Pais_col, Sector, Pais)

  
########
#Denominador
#######
  
  # Detectar las columnas numéricas (sectores en las columnas)
  columnas_valores <- names(Numerador_BISO)[sapply(Numerador_BISO, is.numeric)]
  
  # Si ya existe una columna llamada "Sector", la renombramos temporalmente
  if ("Sector" %in% names(Numerador_BISO)) {
    Numerador_BISO <- Numerador_BISO %>%
      rename(Sector_Fila = Sector)  # Renombrar temporalmente para evitar conflicto
  }
  
  # Convertir a formato largo para manejar sectores en filas
  Denominador_BISO <- Numerador_BISO %>%
    pivot_longer(cols = all_of(columnas_valores), names_to = "Sector_Columna", values_to = "Valor") %>%  # Evitamos el conflicto de nombres
    group_by(Pais_col, Sector_Fila, Sector_Columna) %>%  # Agrupar por país, sector en filas y sector en columnas
    mutate(Valor = if_else(
      Pais == Pais_col, 
      0,00001,  # Si el país coincide con su columna, ponemos 0
      sum(Valor[Pais != Pais_col], na.rm = TRUE)  # Sumamos valores dentro del sector, excluyendo el país actual
    )) %>%
    pivot_wider(names_from = Sector_Columna, values_from = Valor) %>%  # Volver a formato ancho con sectores como columnas
    ungroup()  # Desagrupar

######
#Fracción
  #######
  Numerador_BISO_mut<- Numerador_BISO [,-c(1:3)]
  Denominador_BISO_mut<- Denominador_BISO [,-c(1:3)]
  BISO_raw<-Numerador_BISO_mut/Denominador_BISO_mut
  
# Extraer las 4 primeras columnas del Numerador original
  Columnas_Adicionales <- Numerador_BISO[, 1:3]

# Volver a combinar las columnas con cbind()
  BISO<- cbind(Columnas_Adicionales,BISO_raw)

  BISO<-BISO %>%
    mutate(Pais=factor(Pais,levels = Country)) %>% 
    mutate(Sector_Fila=factor(Sector_Fila, levels = sectores_prioritarios)) %>% 
    arrange(Pais, Sector_Fila)
  BISO<-ifelse(BISO==0,0.0001, BISO)
  write.xlsx(as.data.frame(BISO), "./Base_Import_share_by_origin/BISO.xlsx")
  