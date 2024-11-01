library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(DT)
library(pool)
library(RPostgres)
library(openxlsx)

# Define los nombres de los meses en español
month_names <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Conexión a la base de datos
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
    tags$head(
        tags$link(rel = "stylesheet", 
                  type = "text/css",
                  href = "styles.css")
    ),
    titlePanel("Reporte de Homicidios Intencionales"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            class = "sidebarPanel",
            radioButtons("include_cantons", 
                         label = "¿Desea agregar cantones específicos al reporte?", 
                         choices = c("No", "Sí"), 
                         selected = "No"),
            
            pickerInput("province", 
                        label = "Seleccione la(s) provincia(s)", 
                        choices = NULL, 
                        multiple = TRUE, 
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            
            conditionalPanel(
                condition = "input.include_cantons == 'Sí'",
                pickerInput("canton", 
                            label = "Seleccione el/los cantón(es)", 
                            choices = NULL, 
                            multiple = TRUE, 
                            options = list(`actions-box` = TRUE, `live-search` = TRUE))
            ),
            
            pickerInput("year", 
                        label = "Seleccione el año", 
                        choices = NULL, 
                        multiple = TRUE, 
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            
            pickerInput("month", 
                        label = "Seleccione el/los mes(es)", 
                        choices = month_names, 
                        multiple = TRUE, 
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            
            actionButton("generate_report", "Generar Reporte", icon = icon("play")),
            shinyjs::useShinyjs(),
            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
            actionButton("reset_filters", "Refrescar Filtros"),
            downloadButton("download_excel", "Descargar en Excel", icon = icon("download"))
        ),
        
        mainPanel(
            width = 6,
            class = "mainPanel",
            div(style = "overflow-x: scroll; max-width: 90%;",
                DTOutput("tabla_resultados")
            )
        )
    )
)

# SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
    # Refrescar la página con el botón de reset
    observeEvent(input$reset_filters, {
        shinyjs::js$refresh_page()
    })
    
    # Data reactiva
    infracciones_data <- reactive({
        con <- poolCheckout(pool)
        on.exit(poolReturn(con), add = TRUE) # Asegurar que la conexión se devuelva al pool
        query <- "SELECT * FROM data_lake.hi_total"
        df <- dbGetQuery(con, query)
        df <- df %>% mutate(fecha_infraccion = as.Date(fecha_infraccion))
        return(df)
    })
    
    # Actualizar el pickerInput de Año y Provincia después de cargar los datos
    observe({
        df <- infracciones_data()
        
        # Asegúrate de que haya datos cargados antes de actualizar los selectores
        req(df)
        
        updatePickerInput(session, "year", choices = unique(format(df$fecha_infraccion, "%Y")))
        updatePickerInput(session, "province", choices = unique(df$provincia))
    })
    
    # Actualizar el pickerInput de Cantón en función de la selección de Provincia
    observeEvent(input$province, {
        req(input$province)  # Asegura que la provincia esté seleccionada
        df <- infracciones_data()
        
        # Filtrar los cantones según las provincias seleccionadas
        cantones_filtrados <- df %>%
            filter(provincia %in% input$province) %>%
            pull(canton) %>%
            unique()
        
        updatePickerInput(session, "canton", choices = cantones_filtrados, selected = NULL)
    })
    
    # Crear los datos del reporte al hacer clic en "Generar Reporte"
    report_data <- eventReactive(input$generate_report, {
        req(infracciones_data()) # Asegura que los datos estén cargados
        df <- infracciones_data()
        
        # Filtrar por año, provincia, mes y cantón si están seleccionados
        if (!is.null(input$year) && length(input$year) > 0) {
            df <- df %>% filter(format(fecha_infraccion, "%Y") %in% input$year)
        }
        
        if (!is.null(input$province) && length(input$province) > 0) {
            df <- df %>% filter(provincia %in% input$province)
        }
        
        if (!is.null(input$month) && length(input$month) > 0) {
            selected_months <- match(input$month, month_names) # Meses seleccionados
            df <- df %>% filter(month(fecha_infraccion) %in% selected_months)
        }
        
        if (input$include_cantons == "Sí" && !is.null(input$canton) && length(input$canton) > 0) {
            df <- df %>% filter(canton %in% input$canton)
        }
        
        # Agrupar y resumir
        df <- df %>%
            mutate(AÑO = format(fecha_infraccion, "%Y"),
                   MES = factor(month(fecha_infraccion), levels = 1:12, labels = month_names)) %>%
            group_by(AÑO, MES, provincia, canton) %>%
            summarise(`TOTAL HI` = n(), .groups = "drop") %>%
            rename(CANTÓN = canton, PROVINCIA = provincia)
        
        return(df)
    })
    
    # Mostrar tabla de resultados
    output$tabla_resultados <- renderDT({
        datatable(report_data(), options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
    })
    
    # Descargar el reporte en Excel
    output$download_excel <- downloadHandler(
        filename = function() {
            paste("Reporte_Homicidios_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(report_data(), file)
        }
    )
    
    # Restablecer todos los filtros con el botón "Refrescar Filtros"
    observeEvent(input$reset_filters, {
        updateRadioButtons(session, "include_cantons", selected = "No")
        updatePickerInput(session, "province", selected = NULL)
        updatePickerInput(session, "canton", selected = NULL)
        updatePickerInput(session, "year", selected = NULL)
        updatePickerInput(session, "month", selected = NULL)
    })
}

shinyApp(ui = ui, server = server)
