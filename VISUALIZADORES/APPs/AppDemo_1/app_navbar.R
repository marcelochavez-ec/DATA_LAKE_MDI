library(shiny)

# UI -----------------------------------------------------------------
ui <- navbarPage(
    title = tags$div(
        tags$img(src = "logo_mdi.png",
                 alt = "Logo", 
                 style = "border-radius: 8px; width: 300px; margin-right: 15px;"),
        tags$span("Sistema Estadístico de Información de la Seguridad", 
                  style = "font-size: 18px; color: white;"),
        style = "display: flex; align-items: center;"
    ),
    id = "nav",
    
    # Estilo CSS para el menú
    tags$head(
        tags$style(HTML("
      .navbar {
        background-color: #4C3DA8; /* Color de fondo del menú */
        padding: 30px 20px; /* Aumentar el padding vertical del navbar */
        border-radius: 8px; /* Esquinas redondeadas */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Efecto 3D */
        margin: 20px; /* Margen alrededor del navbar */
      }
      .navbar-brand {
        display: flex; /* Usar flexbox para el logo */
        align-items: center; /* Centrar verticalmente */
      }
      .navbar-nav {
        margin-left: auto; /* Justificar a la derecha */
      }
      .navbar-brand img {
        border-radius: 8px; /* Esquinas redondeadas del logo */
      }
      .navbar-nav > li > a {
        color: white !important; /* Color del texto de los enlaces */
        padding: 10px 15px; /* Padding en los enlaces */
        transition: background-color 0.3s ease; /* Transición suave para el fondo */
        background-color: #4C3DA8; /* Fondo de las opciones del menú */
      }
      .navbar-nav > li > a:hover {
        background-color: #3A2A84 !important; /* Fondo al pasar el ratón */
        color: white !important; /* Color del texto al pasar el ratón */
      }
      .navbar-nav > li.active > a {
        background-color: #4C3DA8 !important; /* Mantener el color activo */
        color: white !important; /* Color del texto activo */
      }
      .navbar-nav .dropdown-menu {
        background-color: #FFFFFF; /* Fondo del menú desplegable */
      }
      .navbar-nav .dropdown-menu > li > a {
        color: #3A2A84; /* Color del texto de las opciones del menú desplegable */
        transition: background-color 0.3s ease; /* Transición suave para el fondo */
      }
      .navbar-nav .dropdown-menu > li > a:hover {
        background-color: #3A2A84; /* Fondo al pasar el ratón en el menú desplegable */
        color: white; /* Color del texto al pasar el ratón */
      }
    "))
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
