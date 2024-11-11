rm(list = ls(all.names = T))

library(R6)
library(DBI)
library(RPostgreSQL)  # Asegúrate de tener el paquete correcto
library(dplyr)
library(readr)
library(stringr)

ConsignaDiariaHI <- R6Class(
    "ConsignaDiariaHI",
    public = list(
        db_con = NULL,
        directory = NULL,
        table_name = NULL,
        schema = NULL,
        
        initialize = function(directory, dbname, host, port, user, password, schema, table_name) {
            self$directory <- directory
            self$table_name <- table_name
            self$schema <- schema
            
            # Conexión a PostgreSQL
            self$db_con <- dbConnect(
                RPostgreSQL::PostgreSQL(),
                dbname = dbname,
                host = host,
                port = port,
                user = user,
                password = password
            )
        },
        
        load_files = function() {
            # Obtener todos los archivos CSV en el directorio especificado
            files <- list.files(self$directory, pattern = "MDI_DES_V1_HOMICIDIOS_INTENCIONALES_\\d{8}_DEES\\.csv$", full.names = TRUE)
            
            # Leer todos los archivos y apilarlos en un solo DataFrame
            all_data <- lapply(files, self$read_and_process_file)
            
            # Indicar cuántos archivos se cargaron
            cat(paste("Se cargaron un total de", length(files), "archivos.\n"))
            
            # Apilar los dataframes y mostrar cuántos registros y columnas tiene el dataframe final
            final_data <- self$rbind_multiple(all_data)
            cat(paste("Se unieron", length(all_data), "archivos y el DataFrame final tiene", nrow(final_data), "registros y", ncol(final_data), "columnas.\n"))
            
            # Guardar el DataFrame combinado en la base de datos
            self$save_to_db(final_data)
            
            # Mensaje final de confirmación de carga en PostgreSQL
            cat(paste("Los datos se han cargado exitosamente en PostgreSQL en la base de datos:", dbGetInfo(self$db_con)$dbname, 
                      "esquema:", self$schema, "tabla:", self$table_name, "\n"))
        },
        
        read_and_process_file = function(file) {
            # Leer el archivo CSV con el delimitador correcto y codificación
            df <- read_delim(file, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE)
            
            # Extraer la fecha del nombre del archivo usando regex
            date_str <- str_extract(basename(file), "\\d{8}")
            date <- as.Date(date_str, format = "%Y%m%d")
            
            # Agregar la columna de fecha al DataFrame
            df <- df %>%
                mutate(fecha_archivo = date)
            
            return(df)
        },
        
        # Función para combinar múltiples dataframes y manejar columnas faltantes
        rbind_multiple = function(dfs) {
            # Encontrar todas las columnas únicas presentes en todos los dataframes
            todas_columnas <- unique(unlist(lapply(dfs, colnames)))
            
            # Asegurar que cada dataframe tenga todas las columnas, llenando con NA donde falten
            dfs_completados <- lapply(dfs, function(df) {
                faltantes <- setdiff(todas_columnas, colnames(df))
                df[faltantes] <- NA
                return(df)
            })
            
            # Usar bind_rows para combinar todos los dataframes
            return(bind_rows(dfs_completados))
        },
        
        save_to_db = function(df) {
            # Verificar si la tabla existe
            table_id <- DBI::Id(schema = self$schema, table = self$table_name)
            
            if (!dbExistsTable(self$db_con, table_id)) {
                # Si no existe, crearla (puedes personalizar esto según el esquema de la tabla)
                dbWriteTable(
                    conn = self$db_con,
                    name = table_id,
                    value = df,
                    row.names = FALSE
                )
            } else {
                # Si la tabla existe, solo agregar los datos
                dbWriteTable(
                    conn = self$db_con,
                    name = table_id,
                    value = df,
                    append = TRUE,  # Cambiar a append para agregar los datos sin sobreescribir
                    row.names = FALSE
                )
            }
        },
        
        finalize = function() {
            dbDisconnect(self$db_con)
        }
    )
)

# Crear una instancia de la clase y ejecutar el proceso
loader <- ConsignaDiariaHI$new(
    directory = "OE_HI/DB/ConsignaDiariaHomicidiosIntencionales",
    dbname = "mdi_dwh",
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = "marce",
    schema = "data_lake",
    table_name = "hi_consigna_diaria"
)

# Cargar y guardar los archivos
loader$load_files()

# Finalizar la conexión
loader$finalize()
