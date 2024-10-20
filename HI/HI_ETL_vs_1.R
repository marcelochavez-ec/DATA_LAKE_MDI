rm(list=ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(RPostgreSQL)

df_hi <- read.xlsx("HI/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                   detectDates = T,
                   sheet = "MDI_HomicidiosIntencionales_PM") %>% 
mutate(rango_edad = case_when(
    edad >= 0 & edad <= 10 ~ "0-10 años",
    edad >= 11 & edad <= 20 ~ "11-20 años",
    edad >= 21 & edad <= 30 ~ "21-30 años",
    edad >= 31 & edad <= 40 ~ "31-40 años",
    edad >= 41 & edad <= 50 ~ "41-50 años",
    edad >= 51 & edad <= 60 ~ "51-60 años",
    edad >= 61 & edad <= 70 ~ "61-70 años",
    edad >= 71 & edad <= 80 ~ "71-80 años",
    edad >= 81 ~ "81 años en adelante"
),
total=1,
mayores_menores = ifelse(edad>=18,"Mayores de edad","Menores de edad"))


df_hi_map <- df_hi %>% 
    mutate(codigo_provincia = case_when(
        nchar(codigo_provincia) == 1 & codigo_provincia %in% 1:9 ~ paste0("0", as.character(codigo_provincia)),
        TRUE ~ as.character(codigo_provincia))) %>% 
    group_by(fecha_infraccion,
             codigo_provincia) %>% 
    summarise(total_hi = n(), .groups = "drop")

# Definir los tipos de datos
field_types <- c(
    "tipo_muerte" = "VARCHAR",                       # 1
    "zona" = "VARCHAR",                              # 2
    "subzona" = "VARCHAR",                           # 3
    "distrito" = "VARCHAR",                          # 4
    "circuito" = "VARCHAR",                          # 5
    "codigo_subcircuito" = "VARCHAR",                # 6
    "subcircuito" = "VARCHAR",                       # 7
    "provincia" = "VARCHAR",                         # 8
    "codigo_provincia" = "INTEGER",                  # 9
    "canton" = "VARCHAR",                            # 10
    "codigo_canton" = "VARCHAR",                     # 11
    "coordenada_y" = "FLOAT(2)",                     # 12
    "coordenada_x" = "FLOAT(2)",                     # 13
    "area_hecho" = "VARCHAR",                        # 14
    "lugar" = "VARCHAR",                             # 15
    "tipo_lugar" = "VARCHAR",                        # 16
    "fecha_infraccion" = "DATE",                     # 17
    "hora_infraccion" = "FLOAT(2)",                  # 18
    "arma" = "VARCHAR",                              # 19
    "tipo_arma" = "VARCHAR",                         # 20
    "presunta_motivacion" = "VARCHAR",               # 21
    "presunta_motivacion_observada" = "VARCHAR",     # 22
    "probable_causa_motivada" = "VARCHAR",           # 23
    "edad" = "FLOAT(2)",                             # 24
    "medida_edad" = "VARCHAR",                       # 25
    "sexo" = "VARCHAR",                              # 26
    "genero" = "VARCHAR",                            # 27
    "etnia" = "VARCHAR",                             # 28
    "estado_civil" = "VARCHAR",                      # 29
    "nacionalidad" = "VARCHAR",                      # 30
    "discapacidad" = "VARCHAR",                      # 31
    "profesional_registro_civil" = "VARCHAR",        # 32
    "instruccion" = "VARCHAR",                       # 33
    "antecedentes" = "VARCHAR",                      # 34
    "rango_edad" = "VARCHAR",                        # 35
    "total" = "numeric(0)"
)
    
# Conectar a PostgreSQL
postgres <- dbConnect(
    RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",  # Cambiar según configuración
    port = 5432,         # Puerto de PostgreSQL
    user = "postgres",   # Usuario de PostgreSQL
    password = "marce"  # Contraseña de PostgreSQL
)

# Guardar la tabla en PostgreSQL con los tipos de columnas especificados
dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_2024"),
    value = df_hi,
    overwrite = TRUE,
    row.names = FALSE
)


# Guardar la tabla en PostgreSQL con los tipos de columnas especificados
dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_2024_map"),
    value = df_hi_map,
    overwrite = TRUE,
    row.names = FALSE
)

# Cerrar la conexión
dbDisconnect(postgres)