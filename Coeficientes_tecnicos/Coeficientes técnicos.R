library(tidyverse)
library(readxl)
library(openxlsx)

###################
# CARGA DE DATOS
###################
load("Data/mis_sectores.RData")
load("Data/Data_origin_UNIZAR.RData")

matrix_io <- data_origin[1:2206, ]

###################
# 1) COEFICIENTES TÉCNICOS: sector_consumption / column_totals
###################
numeric_cols <- colnames(matrix_io)[-c(1, 2)]

column_totals <- matrix_io %>%
  summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)))

sector_consumption <- matrix_io %>%
  group_by(Sector) %>%
  summarise(across(-Pais, \(x) sum(x, na.rm = TRUE)))

text_column    <- sector_consumption[[1]]
numeric_matrix <- as.matrix(sector_consumption[, -1])
numeric_vector <- as.numeric(column_totals[1, ])

divided_matrix <- sweep(numeric_matrix, 2, numeric_vector, "/")

# Exportar matriz intermedia
final_matrix <- cbind(Text = text_column, as.data.frame(divided_matrix))
write.xlsx(final_matrix, "./Coeficientes_tecnicos/Final_matrix_CT_1.xlsx")

###################
# 2) TRANSPOSICIÓN: pivotar país como fila
###################
ct_long <- final_matrix %>%
  mutate(across(-Text, as.numeric)) %>%
  rename_with(~ str_replace_all(., fixed("CZECH_REPUBLIC"), "CZECHREPUBLIC")) %>%
  rename_with(~ gsub("[0-9]", "", .)) %>%
  pivot_longer(
    cols = -Text,
    names_to  = c("Country", ".value"),
    names_pattern = "([^_]*)_(.*)"
  ) %>%
  relocate(Country, .before = Text)

###################
# 3) ORDENAR Y EXPORTAR
###################
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

Country_order <- c(
  "AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECHREPUBLIC", "DENMARK",
  "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
  "IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS",
  "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN",
  "UK", "CHINA", "EASOC", "INDIA", "LATAM", "RUSSIA", "USMCA", "LROW"
)

df_final <- ct_long %>%
  mutate(
    Country = factor(Country, levels = Country_order),
    Text    = factor(Text, levels = sector_order)
  ) %>%
  filter(!is.na(Text) & Text != "") %>%
  arrange(Country, Text)

write.xlsx(as.data.frame(df_final), "./Coeficientes_tecnicos/Tecnical coefficients final.xlsx")

