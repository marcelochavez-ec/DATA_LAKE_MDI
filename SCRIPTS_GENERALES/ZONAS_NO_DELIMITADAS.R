# Instala los paquetes si no los tienes instalados
# install.packages(c("sf", "dplyr"))
rm(list = ls(all.names = T))

# Carga las librerías
library(sf)
library(dplyr)

# 1. Cargar el shapefile de Ecuador (polígonos) en EPSG:4326
ecuador_poligonos <- st_read("C:/Users/marcelochavez/Documents/MDI/DATA_LAKE_MDI/SHAPES/DPA_CORRECCION_INEC/SHAPE_ZONAS_SENPLADES_EPSG4326/ZONAS_SENPLADES_EPSG4326.shp") %>%
    st_transform(crs = 4326) # Asegurarse de que está en EPSG:4326


ecuador_poligonos <- ecuador_poligonos %>% 
    filter(PARROQUIA_ %in% c("LAS GOLONDRINAS",
                             "MANGA DEL CURA",
                             "EL PIEDRERO"))

# 2. Cargar el CSV con latitud y longitud
datos_puntos <- read.csv("C:/Users/marcelochavez/Documents/MDI/DATA_LAKE_MDI/SHAPES/DPA_CORRECCION_INEC/MDI_DES_V1_HOMICIDIOS_INTENCIONALES_DEES.csv", sep = ";") %>% 
mutate(
    coord_x_pn = as.numeric(gsub(",", ".", coord_x_pn)),
    coord_y_pn = as.numeric(gsub(",", ".", coord_y_pn))
)

# 3. Convertir el DataFrame de puntos a objeto sf (EPSG:4326)
puntos_sf <- st_as_sf(datos_puntos, coords = c("coord_y_pn", "coord_x_pn"), crs = 4326)

# 4. Realizar la intersección espacial y asignar atributos del polígono al DataFrame de puntos
puntos_con_atributos <- st_join(puntos_sf, ecuador_poligonos, left = FALSE)

# 5. Verifica el resultado
print(puntos_con_atributos)
