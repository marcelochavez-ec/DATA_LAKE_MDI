library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(DT)
library(pool)
library(RPostgres)

# Conexión a la base de datos (modificar según tu configuración)
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "mdi_dwh",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "marce"
)

# UI -----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Visualización de infracciones por año"),
  sidebarLayout(
    sidebarPanel(
      # Pregunta inicial sobre si desea agregar cantones específicos
      radioButtons("include_cantons", 
                   label = "¿Desea agregar cantones específicos al reporte?", 
                   choices = c("No", "Sí"), 
                   selected = "No"),
      
      # Selección de Provincia
      pickerInput("province", 
                  label = "Seleccione la(s) provincia(s)", 
                  choices = NULL, 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      
      # Selección de Cantón: solo se activa si se desea agregar cantones
      conditionalPanel(
        condition = "input.include_cantons == 'Sí'",
        pickerInput("canton", 
                    label = "Seleccione el/los cantón(es)", 
                    choices = NULL, 
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      ),
      
      # Filtro de selección de año
      pickerInput("year", 
                  label = "Seleccione el año", 
                  choices = NULL, 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      
      # Nuevo Filtro de Selección de Meses
      pickerInput("month", 
                  label = "Seleccione el/los mes(es)", 
                  choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"), 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `live-search` = TRUE))
    ),
    
    mainPanel(
      # Tabla interactiva
      DTOutput("tabla_resultados")
    )
  )
)

# SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Leer los datos de la tabla directamente desde PostgreSQL
  infracciones_data <- reactive({
    con <- poolCheckout(pool)
    query <- "SELECT * FROM data_lake.hi_2024"
    df <- dbGetQuery(pool, query)
    df <- df %>% mutate(fecha_infraccion = as.Date(fecha_infraccion))
    poolReturn(con)  # Asegura que la conexión se devuelve al pool
    return(df)
  })
  
  # Obtener la lista de provincias y años al iniciar la app
  observe({
    df <- infracciones_data()
    
    # Actualizar las opciones de años en el pickerInput
    years <- unique(format(df$fecha_infraccion, "%Y"))
    updatePickerInput(session, "year", choices = years)
    
    # Obtener provincias únicas y actualizar el selector
    provinces <- unique(df$provincia)
    updatePickerInput(session, "province", choices = provinces)
  })
  
  # Actualizar los cantones en función de las provincias seleccionadas
  observeEvent(input$province, {
    df <- infracciones_data()
    
    # Filtrar por provincias seleccionadas para obtener los cantones disponibles
    cantones_filtrados <- df %>%
      filter(provincia %in% input$province) %>%
      pull(canton) %>%
      unique()
    
    # Actualizar el pickerInput de cantones con las opciones disponibles
    updatePickerInput(session, "canton", choices = cantones_filtrados)
  })
  
  # Generar la tabla interactiva con DT
  output$tabla_resultados <- renderDT({
    req(infracciones_data())
    df <- infracciones_data()
    
    # Aplicar los filtros según las selecciones de año, provincias y meses
    if (!is.null(input$year) && length(input$year) > 0) {
      df <- df %>% filter(format(fecha_infraccion, "%Y") %in% input$year)
    }
    if (!is.null(input$province) && length(input$province) > 0) {
      df <- df %>% filter(provincia %in% input$province)
    }
    
    # Filtrar por meses seleccionados si hay alguno
    if (!is.null(input$month) && length(input$month) > 0) {
      month_names <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                       "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
      selected_months <- which(month_names %in% input$month)  # Convertir nombres a índices
      df <- df %>% filter(month(fecha_infraccion) %in% selected_months)
    }
    
    # Si se seleccionan cantones específicos, aplicar el filtro adicional
    if (input$include_cantons == "Sí" && !is.null(input$canton) && length(input$canton) > 0) {
      # Filtro adicional para cantones específicos
      df_cantones <- df %>% filter(canton %in% input$canton)
      
      # Agrupación por provincia y cantón específicos
      tabla_cantones <- df_cantones %>%
        mutate(AÑO = format(fecha_infraccion, "%Y"),
               MES = factor(month(fecha_infraccion), 
                            levels = 1:12, 
                            labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                       "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))) %>%
        group_by(AÑO, MES, provincia, canton) %>%
        summarise(`Total HI` = n(), .groups = "drop") %>%
        rename(Cantón = canton, Provincia = provincia)
      
      # Agrupación de las provincias seleccionadas sin detalle de cantón
      df_provincias <- df %>% 
        filter(!canton %in% input$canton) %>% 
        group_by(AÑO = format(fecha_infraccion, "%Y"), 
                 MES = factor(month(fecha_infraccion), 
                              levels = 1:12, 
                              labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                         "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")), 
                 provincia) %>%
        summarise(`Total HI` = n(), .groups = "drop") %>%
        mutate(Cantón = NA_character_) %>%
        rename(Provincia = provincia)
      
      # Unir ambas tablas para mostrar la estructura combinada
      tabla_final <- bind_rows(df_provincias, tabla_cantones)
      
    } else {
      # Agrupación simple por provincias sin desagregar por cantón
      tabla_final <- df %>%
        mutate(AÑO = format(fecha_infraccion, "%Y"),
               MES = factor(month(fecha_infraccion), 
                            levels = 1:12, 
                            labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                       "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))) %>%
        group_by(AÑO, MES, provincia) %>%
        summarise(`Total HI` = n(), .groups = "drop") %>%
        mutate(Cantón = NA_character_) %>%
        rename(Provincia = provincia)
    }
    
    # Mostrar la tabla con las columnas seleccionadas y ordenadas
    datatable(tabla_final %>% select(AÑO, MES, Provincia, Cantón, `Total HI`), 
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
