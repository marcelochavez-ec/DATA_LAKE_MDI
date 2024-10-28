source("global.R")

# UI -----------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(HTML("<title>SIMDI</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Título de la aplicación
  titlePanel(div(class = "title-panel", "Visualización de infracciones por año")),
  
  # Diseño de la interfaz con múltiples gráficos
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("year", label = "Seleccione el año", choices = NULL)
    ),
    
    mainPanel(
      fluidRow(
        column(
          12,
          div(class = "custom-div",
              id = "histogram-container",
              highchartOutput("histogram", height = "400px"),
              style = "border: 1px solid #ddd; padding: 10px; margin: 10px; border-radius: 8px;"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(class = "custom-div",
              id = "pie-container",
              fluidRow(
                column(
                  4,
                  div(class = "custom-div",
                      highchartOutput("pie_sexo", height = "300px"),
                      style = "border: 1px solid #ddd; padding: 10px; margin: 10px; border-radius: 8px; background-color: #f9f9f9;"
                  )
                ),
                column(
                  4,
                  div(class = "custom-div",
                      highchartOutput("pie_area_del_hecho", height = "300px"),
                      style = "border: 1px solid #ddd; padding: 10px; margin: 10px; border-radius: 8px; background-color: #f9f9f9;"
                  )
                ),
                column(
                  4,
                  div(class = "custom-div",
                      highchartOutput("pie_tipo_lugar", height = "300px"),
                      style = "border: 1px solid #ddd; padding: 10px; margin: 10px; border-radius: 8px; background-color: #f9f9f9;"
                  )
                )
              ),
              style = "border: 1px solid #ddd; padding: 10px; margin: 10px; border-radius: 8px;"
          )
        )
      )
    )
  )
)

# SERVER -----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Leer los datos de la tabla directamente desde PostgreSQL
  infracciones_data <- reactive({
    con <- poolCheckout(pool)
    # on.exit(poolReturn(con))  # Esto garantiza que se devuelva la conexión incluso si hay un error
    query <- "SELECT * FROM data_lake.hi_2024"
    df <- dbGetQuery(pool, query)
    df <- df %>% mutate(fecha_infraccion = as.Date(fecha_infraccion))
    return(df)
  })

  # Actualizar las opciones de años en el selectInput
  observe({
    df <- infracciones_data()
    years <- unique(format(df$fecha_infraccion, "%Y"))
    updateSelectInput(session, "year", choices = years)
  })
  
  # Generar el histograma usando highcharter
  output$histogram <- renderHighchart({
    req(input$year)
    df <- infracciones_data()
    data_filtered <- df %>% 
      filter(format(fecha_infraccion, "%Y") == input$year) %>%
      mutate(month = format(fecha_infraccion, "%Y-%m")) %>%
      count(month)
    
    highchart() %>%
      hc_title(text = paste("Histograma de infracciones en", input$year)) %>%
      hc_xAxis(categories = data_filtered$month, title = list(text = "Mes de infracción")) %>%
      hc_yAxis(title = list(text = "Cantidad de infracciones")) %>%
      hc_add_series(name = "Infracciones", data = data_filtered$n, type = "column") %>%
      hc_tooltip(pointFormat = "Mes: {point.category} <br> Total HI: {point.y}")
  })
  
# Gráfico de pastel: Sexo
output$pie_sexo <- renderHighchart({
  req(input$year)
  generar_grafico_pie(
    data = infracciones_data(),
    variable = "sexo",
    titulo = "Distribución por Sexo",
    colores = color_sexo,
    anio = input$year
  )
})

# Gráfico de pastel: Área del Hecho
output$pie_area_del_hecho <- renderHighchart({
  req(input$year)
  generar_grafico_pie(
    data = infracciones_data(),
    variable = "area_hecho",
    titulo = "Área del Hecho",
    colores = color_hecho,
    anio = input$year
  )
})

# Gráfico de pastel: Tipo de Lugar
output$pie_tipo_lugar <- renderHighchart({
  req(input$year)
  generar_grafico_pie(
    data = infracciones_data(),
    variable = "tipo_lugar",
    titulo = "Tipo de lugar",
    colores = color_lugar,
    anio = input$year
  )
})


}

shinyApp(ui = ui, server = server)
