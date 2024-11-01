library(shiny)
library(R6)
library(DBI)
library(RPostgres)
library(dplyr)
library(tidyr)
library(openxlsx)
library(DT)
library(dbplyr)

# Clase R6 para gestionar la conexión y consultas a PostgreSQL
DataHandler <- R6Class("DataHandler",
                       public = list(
                           conn = NULL,
                           
                           initialize = function(dbname, host, port, user, password) {
                               self$conn <- dbConnect(
                                   RPostgres::Postgres(),
                                   dbname = dbname,
                                   host = host,
                                   port = port,
                                   user = user,
                                   password = password
                               )
                           },
                           
                           get_schemas = function() {
                               dbGetQuery(self$conn, "
        SELECT schema_name 
        FROM information_schema.schemata 
        WHERE schema_name NOT IN ('information_schema', 'pg_catalog')
      ")$schema_name
                           },
                           
                           get_tables = function(schema) {
                               dbGetQuery(self$conn, sprintf("SELECT table_name FROM information_schema.tables WHERE table_schema = '%s'", schema))$table_name
                           },
                           
                           get_variables = function(schema, table) {
                               dbListFields(self$conn, Id(schema = schema, table = table))
                           },
                           
                           fetch_data = function(schema, table, dimensions) {
                               tbl(self$conn, in_schema(schema, table)) %>%
                                   group_by(across(all_of(dimensions))) %>%
                                   summarise(Conteo = n(), .groups = 'drop') %>%
                                   collect()
                           },
                           
                           close_connection = function() {
                               dbDisconnect(self$conn)
                           }
                       )
)

# Interfaz de usuario de Shiny
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
                font-family: 'Calibri Light', sans-serif;
            }
            h1, h2, h3, h4, h5, h6 {
                font-family: 'Calibri Light', sans-serif;
            }
        "))
  ),
  titlePanel("Cubos de las Operaciones Estadísticas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("schema", "Esquema:", choices = NULL),
      uiOutput("tableSelect"),
      uiOutput("dimensionSelect"),
      uiOutput("pivotRowSelect"),
      uiOutput("pivotColSelect"),
      dateRangeInput("dateRange", "Rango de Fechas:", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("refresh", "Consultar"),
      actionButton("reset", "Reiniciar"),
      downloadButton("download", "Descargar Excel")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Servidor de Shiny
server <- function(input, output, session) {
  dataHandler <- DataHandler$new(
    dbname = "mdi_dwh",
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = "marce"
  )
  
  observe({
    schemas <- dataHandler$get_schemas()
    updateSelectInput(session, "schema", choices = schemas)
  })
  
  observeEvent(input$schema, {
    req(input$schema)
    tables <- dataHandler$get_tables(input$schema)
    updateSelectInput(session, "table", choices = tables)
  })
  
  observeEvent(input$table, {
    req(input$schema, input$table)
    variables <- dataHandler$get_variables(input$schema, input$table)
    updateSelectInput(session, "dimensions", choices = variables)
  })
  
  output$tableSelect <- renderUI({
    selectInput("table", "Tabla:", choices = NULL)
  })
  
  output$dimensionSelect <- renderUI({
    selectInput("dimensions", "Dimensiones:", choices = NULL, multiple = TRUE)
  })
  
  output$pivotRowSelect <- renderUI({
    req(input$dimensions)
    selectInput("pivotRow", "Pivoteo en filas:", choices = input$dimensions, selected = NULL, multiple = TRUE)
  })
  
  output$pivotColSelect <- renderUI({
    req(input$dimensions)
    selectInput("pivotCol", "Pivoteo en columnas:", choices = input$dimensions, selected = NULL, multiple = TRUE)
  })
  
  result_data <- eventReactive(input$refresh, {
    req(input$schema, input$table, input$dimensions, input$dateRange)
    
    # Obtener datos y aplicar el filtro de fechas
    data <- dataHandler$fetch_data(input$schema, input$table, input$dimensions) %>%
      filter(between(fecha_infraccion, input$dateRange[1], input$dateRange[2]))  # Ajusta 'fecha_infraccion' al nombre de tu columna de fecha
    
    row_vars <- input$pivotRow
    col_vars <- input$pivotCol
    
    if (length(row_vars) > 0 && length(col_vars) > 0) {
      pivot_data <- data %>%
        pivot_wider(names_from = all_of(col_vars), values_from = Conteo, values_fill = list(Conteo = 0))
      return(pivot_data)
    } else {
      return(data)
    }
  })
  
  output$table <- renderDT({
    req(result_data())
    datatable(result_data())
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "schema", selected = NULL)
    updateSelectInput(session, "table", choices = NULL, selected = NULL)
    updateSelectInput(session, "dimensions", choices = NULL, selected = NULL)
    updateSelectInput(session, "pivotRow", selected = NULL)
    updateSelectInput(session, "pivotCol", selected = NULL)
    updateDateRangeInput(session, "dateRange", start = Sys.Date() - 30, end = Sys.Date())
  })
  
  output$download <- downloadHandler(
    filename = function() { paste("reporte_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      export_data <- result_data()
      numeric_cols <- sapply(export_data, is.integer) | sapply(export_data, is.numeric)
      export_data[numeric_cols] <- lapply(export_data[numeric_cols], as.numeric)
      write.xlsx(export_data, file, rowNames = FALSE)
    }
  )
  
  onSessionEnded(function() {
    dataHandler$close_connection()
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
