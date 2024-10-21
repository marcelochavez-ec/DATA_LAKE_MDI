# Limpiar el espacio de trabajo
rm(list=ls(all.names = TRUE))

library(tidyverse)
library(openxlsx)
library(RPostgreSQL)

# Cargar los datos
df_hi <- read.xlsx("HI/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                   detectDates = TRUE,
                   sheet = "MDI_HomicidiosIntencionales_PM")



# Obtener los tipos de datos reales del dataframe
real_types <- sapply(df_hi, class)

# Comparar los tipos de datos
comparison <- data.frame(
    Column = names(field_types),
    Expected_Type = field_types,
    Actual_Type = real_types,
    Match = field_types == real_types
)

# Mostrar las comparaciones
print(comparison)

# Filtrar las columnas que no coinciden
discrepancies <- comparison %>% filter(!Match)

# Mostrar discrepancias si existen
if (nrow(discrepancies) > 0) {
    print("Discrepancias encontradas:")
    print(discrepancies)
} else {
    print("Todos los tipos de datos coinciden.")
}
