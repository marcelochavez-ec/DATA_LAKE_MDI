library(shiny)

# UI -----------------------------------------------------------------
ui <- navbarPage(
  
  title = tags$div(
    class = "navbar-title",  # Añadir clase CSS para los estilos
    tags$img(src = "logo_mdi.png", alt = "Logo"),
    tags$span("Sistema Estadístico de Información de la Seguridad")
  ),
  
  id = "nav",
  
  # Incluir archivo CSS externo
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Menús desplegables para cada sección principal
  navbarMenu("Homicidios Intencionales",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  ),
  navbarMenu("Armas Ilícitas",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  ),
  navbarMenu("Personas Desaparecidas",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  ),
  navbarMenu("Decomiso de Droga",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  ),
  navbarMenu("Trata de Personas",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  ),
  navbarMenu("Explosivos",
             tabPanel("Documentación Estadística", "Contenido de Documentación Estadística"),
             tabPanel("Cubo", "Contenido del Cubo"),
             tabPanel("Reporte global", "Contenido de Reporte global"),
             tabPanel("Dashboard", "Contenido de Dashboard")
  )
)

# SERVER -----------------------------------------------------------------
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)