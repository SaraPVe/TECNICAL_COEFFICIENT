#install.packages(c("readxl", "tidyr", "dplyr", "openxlsx", "readr"))
# Lectura del excel
library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
#CALCULOS
matrix_io<- read.xlsx("DATA_BIS_ORIGIN.xlsx", colNames = TRUE)
# Identificar columnas numéricas (todas excepto las dos primeras)
  numeric_cols <- colnames(matrix_io)[-c(1, 2)]
# Calcular la suma total por cada columna
column_totals <- matrix_io %>%
summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)))
# Calcular la suma total por cada fila
sector_consumption <- matrix_io %>%
group_by(Sector) %>%
summarise(across(-c(Pais), sum, na.rm = TRUE))  # Excluimos `Country`
# Extraer la primera columna de texto (sin modificar)
text_column <- sector_consumption[[1]]  # La primera columna de texto
numeric_matrix <- as.matrix(sector_consumption[, -1])
numeric_vector <- as.numeric(column_totals[1, ])
# Dimensiones de la matriz numérica
dim(numeric_matrix)  # Debe ser 64 filas x 2170 columnas
# Longitud del vector numérico
length(numeric_vector)  # Debe ser 2170
# Dividir cada columna de la matriz por el vector
divided_matrix <- sweep(numeric_matrix, 2, numeric_vector, "/")
# Combinar la columna de texto con la matriz dividida
final_matrix <- cbind(Text = text_column, divided_matrix)
write.xlsx(as.data.frame(final_matrix), "./Coeficientes_tecnicos/Final_matrix_CT_1.xlsx")
#Trasponemos los datos para que queden en la forma del excel
sector_column <- final_matrix[[1]] #primera columna de los sectores
# Extraer el resto como matriz numérica
data_matrix <- final_matrix[, -1]  # Excluyendo la primera columna
colnames(data_matrix) <- paste0("Country_", seq_len(ncol(data_matrix)))  # Asegurar nombres únicos
# Añadir la columna de sectores a los datos
data_matrix <- cbind(Sector = sector_column, data_matrix)
#Seleccionamos los nombres de los paises y de los sectores
matrix_data <- read_excel("./Coeficientes_tecnicos/Final_matrix_CT_1.xlsx", col_names = TRUE) 


a<-matrix_data %>%
  rename_with(~ str_replace_all(., fixed("CZECH_REPUBLIC"), "CZECHREPUBLIC")) %>% 
mutate(across(
  .cols = -Text,  # Seleccionar todas las columnas excepto "TextColumn"
  .fns = as.numeric     # Transformar a numérico
)) %>% 
    rename_with(~gsub("[0-9]","",.)) %>% #quitar los números
  pivot_longer( #pivatar solamente el pais 
    cols = -Text,         # Todas las columnas excepto "Sector"
    names_to = c("Country", ".value"),    # Nombre para los países
    names_pattern = "([^_]*)_(.*)",) %>%  # cómo se ha pivotado, para pivotar solamente lo que esta en la primera barrabaja
  relocate(Country, .before = Text)  # cambiar orden  sector
write.xlsx(as.data.frame(a), "./Coeficientes_tecnicos/Tecnical coefficients V1.xlsx")
sector_order <- c(
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
Country_order<- c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECHREPUBLIC","DENMARK", 
"ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
"IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS", 
"POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN", 
"UK", "CHINA", "EASOC", "INDIA", "LATAM", "RUSSIA", "USMCA", "LROW")
df<- read.xlsx("./Coeficientes_tecnicos/Tecnical coefficients V1.xlsx")
df <- df %>%
mutate(Country=factor(Country,levels = Country_order),
Text = factor(Text, levels = sector_order)) %>%
arrange(Country, Text)  # Ordenar por país y sector
write.xlsx(as.data.frame(df), "./Coeficientes_tecnicos/Tecnical coefficients final.xlsx")


