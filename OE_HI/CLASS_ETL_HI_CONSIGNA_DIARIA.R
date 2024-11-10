library(R6)
library(DBI)
library(RPostgres)
library(dplyr)
library(readr)
library(stringr)

ConsignaDiariaLoader <- R6Class(
    "ConsignaDiariaLoader",
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
                Postgres(),
                dbname = dbname,
                host = host,
                port = port,
                user = user,
                password = password
            )
        },
        
        load_files = function() {
            files <- list.files(self$directory, pattern = "MDI_DES_V1_HOMICIDIOS_INTENCIONALES_\\d{8}_DEES\\.csv$", full.names = TRUE)
            
            for (file in files) {
                df <- self$read_and_process_file(file)
                self$save_to_db(df)
            }
        },
        
        read_and_process_file = function(file) {
            # Leer el archivo CSV
            df <- read_csv(file)
            
            # Extraer la fecha del nombre del archivo usando regex
            date_str <- str_extract(basename(file), "\\d{8}")
            date <- as.Date(date_str, format = "%Y%m%d")
            
            # Agregar la columna de fecha al DataFrame
            df <- df %>%
                mutate(fecha_archivo = date)
            
            return(df)
        },
        
        save_to_db = function(df) {
            # Guardar en la tabla PostgreSQL especificada
            dbWriteTable(
                conn = self$db_con,
                name = DBI::Id(schema = self$schema, table = self$table_name),
                value = df,
                append = TRUE,
                row.names = FALSE
            )
        },
        
        finalize = function() {
            dbDisconnect(self$db_con)
        }
    )
)
