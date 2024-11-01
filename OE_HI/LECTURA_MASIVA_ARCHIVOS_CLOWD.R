# Limpiar el espacio de trabajo
rm(list = ls(all.names = TRUE))

library(readr)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(stringr)
library(RPostgreSQL)
library(openxlsx)

# Conexión a PostgreSQL:
postgres <- dbConnect(
    RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",  # Cambiar según configuración
    port = 5432,         # Puerto de PostgreSQL
    user = "postgres",   # Usuario de PostgreSQL
    password = "marce"  # Contraseña de PostgreSQL
)

# Función para cargar varios archivos con manejo de errores
cargar_archivos_homicidios <- function(url_base, fecha_inicio, fecha_fin) {
    
    # Generar secuencia de fechas desde el inicio hasta la fecha actual
    fechas <- seq(ymd(fecha_inicio), ymd(fecha_fin), by = "day")
    
    # Generar los nombres de los archivos CSV con base en las fechas
    archivos_csv <- map(fechas, function(fecha) {
        glue("{url_base}MDI_DES_V1_HOMICIDIOS_INTENCIONALES_{format(fecha, '%Y%m%d')}_DEES.csv")
    })
    
    # Leer los archivos CSV y agregar la columna fecha_carga
    df_list <- map(archivos_csv, function(archivo_url) {
        # Intentar leer el archivo con tryCatch
        tryCatch({
            # Leer el archivo sin mostrar el tipo de columnas
            df <- read_delim(archivo_url, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE)
            
            # Extraer la fecha de carga desde el nombre del archivo
            fecha_carga <- str_extract(archivo_url, "\\d{8}")
            
            # Agregar la columna fecha_carga al DataFrame
            df <- df %>% mutate(fecha_carga = ymd(fecha_carga))
            
            # Retornar el data frame cargado
            return(df)
            
        }, error = function(e) {
            # En caso de error, imprimir un mensaje y continuar
            message(glue("Archivo no encontrado o error al cargar: {archivo_url}"))
            return(NULL)  # Retornar NULL para omitir este archivo en caso de error
        })
    })
    
    # Filtrar los elementos NULL que pudieron generarse por errores
    df_list <- compact(df_list)
    
    # Retornar la lista de data frames
    return(df_list)
}

# Parámetros para la función
url_base <- "https://repositorio.ministeriodelinterior.gob.ec/owncloud/index.php/s/xYj3HBiRQ49ig00/download?path=%2F2.%20Uso%20(versiones%20anteriores)%2FHomicidios%20Intencionales&files="
fecha_inicio <- "2024-07-17"
fecha_fin <- Sys.Date()

# Llamar a la función para cargar los archivos
dataframes <- cargar_archivos_homicidios(url_base, fecha_inicio, fecha_fin)

# Función para hacer rbind y manejar columnas faltantes
rbind_multiple <- function(...) {
    # Listar todos los data frames que se pasan a la función
    dfs <- list(...)
    
    # Encontrar todas las columnas únicas presentes en todos los data frames
    todas_columnas <- unique(unlist(lapply(dfs, colnames)))
    
    # Asegurar que cada data frame tenga todas las columnas, llenando con NA donde falten
    dfs_completados <- lapply(dfs, function(df) {
        # Añadir cualquier columna faltante como NA
        faltantes <- setdiff(todas_columnas, colnames(df))
        df[faltantes] <- NA
        return(df)
    })
    
    # Usar bind_rows para combinar todos los data frames
    return(bind_rows(dfs_completados))
}

df_hi_14_23 <- read.xlsx("OE_HI/DB/mdi_homicidios_intencionales_pm_2014_2023.xlsx",
                        sheet = "MDI_HomicidiosIntencionales_PM", 
                        detectDates = T)

df_hi_2024 <- read.xlsx("OE_HI/DB/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                        sheet = "MDI_HomicidiosIntencionales_PM", 
                        detectDates = T)

df_hi_total <- rbind(df_hi_14_23, df_hi_2024)


dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_total"),
    value = df_hi_total,
    overwrite = TRUE,
    row.names = FALSE
)

# Usar do.call para aplicar la función a la lista de dataframes
df_cs_hi <- do.call(rbind_multiple, df_hi_total)


# Guarda la tabla df_hi_map en PostgreSQL
dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_total"),
    value = df_hi_total,
    overwrite = TRUE,
    row.names = FALSE
)


