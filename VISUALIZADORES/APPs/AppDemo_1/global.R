library(shiny)
library(highcharter)
library(dplyr)
library(pool)
library(DBI)
library(shinythemes)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Crear un pool de conexión a la base de datos
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "mdi_dwh",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "marce"
)

# Crear un vector de colores específico
color_sexo <- c("HOMBRE" = "#778da9",
                "MUJER" = "#ffc2d1",
                "NO DETERMINADO" = "#f7a072")

# Crear un vector de colores específico
color_hecho <- c("RURAL" = "#ffc2d1",
                 "URBANO" = "#778da9")

# Crear un vector de colores específico
color_lugar <- c("PRIVADO" = "#ffc2d1",
                 "PUBLICO" = "#778da9")

# Función para generar gráficos de pastel parametrizados
generar_grafico_pie <- function(data, variable, titulo, colores, anio) {
  # Filtrar por año y contar la variable especificada
  data_filtrada <- data %>%
    filter(format(fecha_infraccion, "%Y") == anio) %>%
    count(!!sym(variable)) %>%
    filter(!is.na(!!sym(variable)))
  
  # Asignar los colores a las categorías de la variable
  data_filtrada$color <- colores[as.character(data_filtrada[[variable]])]
  
  # Crear el gráfico de pastel con opciones personalizadas
  hchart(data_filtrada, "pie", hcaes(name = !!sym(variable), y = n, color = color)) %>%
    hc_title(text = titulo) %>%
    hc_plotOptions(
      pie = list(
        depth = 30,
        innerSize = '25%',
        showInLegend = TRUE,
        slicedOffset = 30,
        allowPointSelect = TRUE,
        dataLabels = list(enabled = FALSE)
      )
    ) %>%
    hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 50, beta = 0)) %>%
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal",
      y = 10,
      itemStyle = list(fontSize = "10px", fontFamily = "Calibri Light")
    ) %>%
    hc_tooltip(pointFormat = '<b style="font-size: 14px;">{point.name}</b><br>Cantidad: {point.y:,.0f}<br>Porcentaje: {point.percentage:.1f}%')
}
