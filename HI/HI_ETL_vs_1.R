rm(list=ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(RPostgreSQL)

df_hi <- read.xlsx("HI/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                   detectDates = T,
                   sheet = "MDI_HomicidiosIntencionales_PM")

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
    "coordenada_y" = "FLOAT(2)",                    # 12
    "coordenada_x" = "FLOAT(2)",                    # 13
    "area_hecho" = "VARCHAR",                        # 14
    "lugar" = "VARCHAR",                             # 15
    "tipo_lugar" = "VARCHAR",                        # 16
    "fecha_infraccion" = "DATE",                    # 17
    "hora_infraccion" = "FLOAT(2)",                 # 18
    "arma" = "VARCHAR",                              # 19
    "tipo_arma" = "VARCHAR",                         # 20
    "presunta_motivacion" = "VARCHAR",               # 21
    "presunta_motivacion_observada" = "VARCHAR",     # 22
    "probable_causa_motivada" = "VARCHAR",           # 23
    "edad" = "FLOAT(2)",                            # 24
    "medida_edad" = "VARCHAR",                       # 25
    "sexo" = "VARCHAR",                              # 26
    "genero" = "VARCHAR",                            # 27
    "etnia" = "VARCHAR",                             # 28
    "estado_civil" = "VARCHAR",                      # 29
    "nacionalidad" = "VARCHAR",                      # 30
    "discapacidad" = "VARCHAR",                      # 31
    "profesional_registro_civil" = "VARCHAR",        # 32
    "instruccion" = "VARCHAR",                       # 33
    "antecedentes" = "VARCHAR"                       # 34
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
    row.names = FALSE,
    field.types = field_types
)

# Cerrar la conexión
dbDisconnect(postgres)