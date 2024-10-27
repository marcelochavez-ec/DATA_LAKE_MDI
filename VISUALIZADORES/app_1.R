library(shiny)
library(R6)
library(DBI)
library(RPostgres)
library(dplyr)
library(tidyr)
library(openxlsx)
library(DT)

# Clase R6 para gestionar la conexión y consultas a PostgreSQL
DataHandler <- R6Class("DataHandler",
                       public = list(
                           conn = NULL,
                           schema = NULL,
                           table = NULL,
                           
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
                           
                           set_schema_table = function(schema, table) {
                               self$schema <- schema
                               self$table <- table
                           },
                           
                           get_tables = function(schema) {
                               dbListTables(self$conn, schema = schema)
                           },
                           
                           get_variables = function() {
                               dbListFields(self$conn, Id(schema = self$schema, table = self$table))
                           },
                           
                           fetch_data = function(dimensions) {
                               tbl(self$conn, in_schema(self$schema, self$table)) %>%
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
    titlePanel("Análisis OLAP con Shiny"),
    sidebarLayout(
        sidebarPanel(
            selectInput("schema", "Esquema:", choices = c("data_lake")),
            uiOutput("tableSelect"),
            uiOutput("dimensionSelect"),
            uiOutput("pivotRowSelect"),
            uiOutput("pivotColSelect"),
            actionButton("refresh", "Refrescar"),
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
    # Instancia de la clase R6 con la conexión a la base de datos
    dataHandler <- DataHandler$new(
        dbname = "mdi_dwh",
        host = "localhost",
        port = 5432,
        user = "postgres",
        password = "marce"
    )
    
    # Configurar el esquema y la tabla predeterminados
    observe({
        dataHandler$set_schema_table("data_lake", "hi_2024")
        variables <- dataHandler$get_variables()
        updateSelectInput(session, "table", choices = "hi_2024", selected = "hi_2024")
        updateSelectInput(session, "dimensions", choices = variables)
    })
    
    # Selector de tabla predeterminado
    output$tableSelect <- renderUI({
        selectInput("table", "Tabla:", choices = "hi_2024")
    })
    
    # Selector de dimensiones dinámico
    output$dimensionSelect <- renderUI({
        selectInput("dimensions", "Dimensiones:", choices = NULL, multiple = TRUE)
    })
    
    # Selector de pivoteo de filas y columnas
    output$pivotRowSelect <- renderUI({
        req(input$dimensions)
        selectInput("pivotRow", "Pivoteo en filas:", choices = input$dimensions, selected = input$dimensions[1])
    })
    
    output$pivotColSelect <- renderUI({
        req(input$dimensions)
        selectInput("pivotCol", "Pivoteo en columnas:", choices = input$dimensions, selected = input$dimensions[2])
    })
    
    # Actualizar variables cuando se selecciona una tabla
    observeEvent(input$table, {
        req(input$table)
        dataHandler$set_schema_table(input$schema, input$table)
        variables <- dataHandler$get_variables()
        updateSelectInput(session, "dimensions", choices = variables)
    })
    
    # Tabla de resultados reactiva
    data <- eventReactive(input$refresh, {
        req(input$dimensions)
        dataHandler$fetch_data(input$dimensions)
    })
    
    # Mostrar tabla de resultados con pivoteo opcional
    output$table <- renderDT({
        req(data())
        if (input$pivotRow %in% input$dimensions && input$pivotCol %in% input$dimensions) {
            # Convertir la tabla en formato ancho (pivot)
            pivot_data <- data() %>%
                pivot_wider(names_from = !!sym(input$pivotCol), values_from = Conteo, values_fill = list(Conteo = 0))
            datatable(pivot_data)
        } else {
            datatable(data())
        }
    })
    
    # Resetear selecciones de variables
    observeEvent(input$reset, {
        updateSelectInput(session, "table", selected = "hi_2024")
        updateSelectInput(session, "dimensions", selected = NULL)
        updateSelectInput(session, "pivotRow", selected = NULL)
        updateSelectInput(session, "pivotCol", selected = NULL)
    })
    
    # Descargar resultados en Excel
    output$download <- downloadHandler(
        filename = function() { paste("reporte_", Sys.Date(), ".xlsx", sep = "") },
        content = function(file) {
            write.xlsx(data(), file)
        }
    )
    
    # Cerrar conexión al finalizar la sesión
    onSessionEnded(function() {
        dataHandler$close_connection()
    })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
