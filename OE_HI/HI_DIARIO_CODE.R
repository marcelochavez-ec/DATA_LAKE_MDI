# ============================================================
# SCRIPT: Automatización de Procesos de Reporte de HI
# AUTOR: Luis Fernando Moreta Cruz
# FECHA DE CREACIÓN: 13-09-2024
# VERSIÓN: 1.0
# DESCRIPCIÓN:
# Este script está diseñado para automatizar el proceso de 
# generación de cuadros de HI diario que sirve para su respectivo reporte. 
# Toma como entrada archivos de descargados de OwnCloud ubicados en HI/IMPORT/DATA_IMPORT 
# y genera resultados en formato tablas den datos en hojas de HI/PLANTILLA/Reporte_homicidios_Plantilla.xlsx.
# ============================================================

# HISTORIAL DE CAMBIOS:
# - 09-09-2024 Versión 1.0: Creación del script

## IMPORTACIÓN DE LIBRERÍAS -------------

t1 <- Sys.time()

library(tidyverse)
library(openxlsx)
library(writexl)
library(readr)
library(readxl)
library(janitor)
library(lubridate)


## CREACIÓN DE VARIABLES GLOBALES-----

# f_hoy: Variable que almacena la fecha actual utilizando la función today() 
f_hoy <- today()

# f_menos2d: Variable que almacena la fecha actual restado dos días, el corte habitual en SP.
f_menos2d <- f_hoy-2

# f_y_today: Obtiene el año actual a partir de la variable f_hoy.
f_y_today <- year(f_hoy)

# f_inicio_y_hoy: Define el 1 de enero del año actual, concatenando el año actual con el formato "-01-01".
f_inicio_y_hoy <- as.Date(paste0(f_y_today, "-01-01"))

# f_y_ant: Almacena el año anterior, restando 1 al año actual.
f_y_ant <- year(f_hoy) - 1

# f_inicio_y_ant: Define el 1 de enero del año anterior, concatenando el año anterior con el formato "-01-01".
f_inicio_y_ant <- as.Date(paste0(f_y_ant, "-01-01"))

f_inicio_y_2014 <- as.Date(paste0(f_y_ant-9, "-01-01"))

# f_final_y_ant: Define el 31 de diciembre del año anterior, concatenando el año anterior con el formato "-12-31".
f_final_y_ant <- as.Date(paste0(f_y_ant, "-12-31"))

# f_hace_un_year: Calcula la fecha de reporte pero hace exactamente un año, restando un año a f_menos2d.
f_hace_un_year <- f_menos2d %m-% years(1)

## IMPORTACIÓN DE DATOS -------------

### CARGAS DIARIAS DE BBDD -------

### REPORTE DIARIO DE HOMICIDIOS INTENCIONALES 2024
D_HI <- read_delim("HI/DATA_IMPORT/Diaria/MDI_DES_V1_HOMICIDIOS_INTENCIONALES_DEES.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)


fechas_semanas_iso_2023_2025 <- read_excel("HI/DATA_IMPORT/Historica/fechas_semanas_iso_2023_2025.xlsx", 
                                           col_types = c("numeric", "numeric", "date", 
                                                         "date"))

fechas_semanas_iso_2023_2025 <- clean_names(fechas_semanas_iso_2023_2025)

### CARGAS REGULAR DE BBDD HISTÓRICA---------


# Función que pregunta si existen nuevas bases históricas
preguntar_nuevas_bases <- function() {
  # Pregunta al usuario si hay nuevas bases históricas
  respuesta <- readline(prompt = "¿Existen nuevas bases históricas? (S/N): ")
  respuesta <- str_to_upper(respuesta)
  # Retorna la respuesta ingresada
  return(respuesta)
}


respuesta_usuario <- preguntar_nuevas_bases()

# Hacer algo en función de la respuesta
if (respuesta_usuario == "S") {
  cat("Hay nuevas bases históricas.\n")

  
  H_HOMICIDIOS <- read_excel("HI/DATA_IMPORT/historica/HI_2014-2023_septiembre2024.xlsx")
  
  

  save( H_HOMICIDIOS,
       file = "HI/DATA_IMPORT/Historica/Datos_hist.RData")
} else {
  cat("No hay nuevas bases históricas.\n")
  
  load("HI/DATA_IMPORT/Historica/Datos_hist.RData")
  
}

## Procesamiento de las bases de datos----------

### Procesamiento de las BBDD HISTÓRICAS -------

#1. H_HOMICIDIOS Y MULTIPLE

H_HOMICIDIOS <- clean_names(H_HOMICIDIOS)





trata_hi_multiple <- function(data, f_inicio, f_final) {
  issub_1 <- is.null(data$sub_zona_1)
  
  if(issub_1){
    data$sub_zona_1 <- data$sub_zona
  }
  
  data %>%
    filter(fecha_infraccion <= f_final, 
           fecha_infraccion >= f_inicio, 
           multiple == "SI", 
           arma == "ARMA DE FUEGO",
           presunta_motivacion != "VIOLENCIA COMUNITARIA",
           presunta_motivacion != "VIOLENCIA INTRAFAMILIAR") %>%
    mutate(sub_zona_1 = str_replace(sub_zona_1, "SANTO DOMINGO DE LOS TSACHILAS", "STO DGO DE LOS TSACHILAS"),
           sub_zona_1 = str_replace(sub_zona_1, "DMQ", "D.M. QUITO"),
           sub_zona_1 = str_replace(sub_zona_1, "DMG", "D.M. GUAYAQUIL"),
           sub_zona_1 = str_replace(sub_zona_1, "ZONA N\\. D\\.", "ZONA NO DELIMITADA"),
           sub_zona_1 = str_replace_all(sub_zona_1, "Í", "I"), # Añadido para reemplazar "Í" por "I"
           antecedentes_siipne = str_replace_all(antecedentes_siipne, " ", ""))  # Eliminar espacios)
}


trata_hi <- function(data, f_inicio, f_final) {
  issub_1 <- is.null(data$sub_zona_1)
  
  if(issub_1){
    data$sub_zona_1 <- data$sub_zona
  }
  
  data %>% filter(fecha_infraccion <= f_final, 
                  fecha_infraccion >= f_inicio) %>% 
    mutate(sub_zona_1 = str_replace(sub_zona_1, "SANTO DOMINGO DE LOS TSACHILAS", "STO DGO DE LOS TSACHILAS"),
           sub_zona_1 = str_replace(sub_zona_1, "DMQ", "D.M. QUITO"),
           sub_zona_1 = str_replace(sub_zona_1, "DMG", "D.M. GUAYAQUIL"),
           sub_zona_1 = str_replace(sub_zona_1, "ZONA N\\. D\\.", "ZONA NO DELIMITADA"),
           sub_zona_1 = str_replace_all(sub_zona_1, "Í", "I"), # Añadido para reemplazar "Í" por "I"
           antecedentes_siipne = str_replace_all(antecedentes_siipne, " ", ""))  # Eliminar espacios
}



H_HOMICIDIOS_MULTIPLE <- H_HOMICIDIOS %>% trata_hi_multiple(f_inicio_y_ant,f_final_y_ant)

H_HOMICIDIOS_MULTIPLE <- H_HOMICIDIOS_MULTIPLE %>% filter(!duplicated(H_HOMICIDIOS_MULTIPLE$codigo_acta))


H_HI_F <- H_HOMICIDIOS %>% trata_hi(f_inicio_y_ant,f_final_y_ant)




### Procesamiento de las BBDD DIARIAS -------

# 1. HI y HI Multiple

D_HI <- clean_names(D_HI)

D_HI_MULTIPLE <- D_HI %>% trata_hi_multiple(f_inicio_y_hoy,f_hoy)

D_HI_MULTIPLE <- D_HI_MULTIPLE %>% filter(!duplicated(D_HI_MULTIPLE$codigo_acta))


D_HI_F <- D_HI %>% trata_hi(f_inicio_y_hoy,f_hoy)


## CREACIÓN DE LAS TABLAS RESUMEN -----------

### I - INDICADORES DE RESULTADOS

### 1. Casos de homicidios múltiples con armas de fuego (HM_01)------


subzonas <- c("ORELLANA", "ZONA NO DELIMITADA", "AZUAY", "SUCUMBIOS", "TUNGURAHUA", 
              "BOLIVAR", "NAPO", "PICHINCHA", "GALAPAGOS", "CARCHI", "ZAMORA CHINCHIPE", 
              "IMBABURA", "CAÑAR", "SANTA ELENA", "LOS RIOS", "CHIMBORAZO", 
              "MORONA SANTIAGO", "COTOPAXI", "LOJA", "PASTAZA", "D.M. QUITO", 
              "MANABI", "STO DGO DE LOS TSACHILAS", "EL ORO", "GUAYAS", 
              "ESMERALDAS", "D.M. GUAYAQUIL")


# Create the DataFrame with empty n_2023 and n_2024 columns
bd_hi_subzonas <- data.frame(LUGAR_GEO = subzonas, n_2023 = NA, n_2024 = NA)

bd_hi_sub_24 <- D_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, fecha_infraccion <= f_menos2d) %>% 
  group_by(sub_zona_1) %>% summarise(n=n())

bd_hi_sub_23 <- H_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, fecha_infraccion <= f_hace_un_year) %>% 
  group_by(sub_zona_1) %>% summarise(n=n())



bd_hi_subzonas <- bd_hi_subzonas %>%
  left_join(bd_hi_sub_23, by = c("LUGAR_GEO" = "sub_zona_1")) %>% 
  left_join(bd_hi_sub_24, by = c("LUGAR_GEO" = "sub_zona_1")) 

bd_hi_subzonas <- bd_hi_subzonas %>%
  mutate(
    n_2023 = ifelse(is.na(n_2023), n.x, n_2023),
    n_2024 = ifelse(is.na(n_2024), n.y, n_2024)
  )
# Reemplazar los valores NA con 0 en caso de que no haya datos para ciertos cantones
bd_hi_subzonas <- bd_hi_subzonas %>%
  mutate(n_2023 = ifelse(is.na(n_2023), 0, n_2023),
         n_2024 = ifelse(is.na(n_2024), 0, n_2024))


# Resultado final
bd_hi_subzonas$n.x <- NULL
bd_hi_subzonas$n.y <- NULL
bd_hi_subzonas$var <- bd_hi_subzonas$n_2024-bd_hi_subzonas$n_2023

bd_hi_subzonas <- bd_hi_subzonas %>% arrange(desc(var))

### 2. Muertes el último día -----


HI_DIA_24 <- D_HI_F %>% filter(fecha_infraccion==f_menos2d) %>% nrow
HI_DIA_23 <- H_HI_F %>% filter(fecha_infraccion==f_hace_un_year) %>% nrow
# 
# bd_hi_subzonas <- bd_hi_subzonas %>% bind_rows(data.frame("LUGAR_GEO"="muerte_dia",n_2023=HI_DIA_23,n_2024=HI_DIA_24))
# 

### 3. Presunta motivación ---
ame_hi_24 <- D_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, fecha_infraccion <= f_menos2d,
        presunta_motivacion_observacion =="AMENAZA") %>% nrow()

traf_hi_24 <- D_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, fecha_infraccion <= f_menos2d,
         presunta_motivacion_observacion %in% c("TRAFICO INTERNACIONAL DE DROGA","TRAFICO INTERNOS DE DROGAS (MICROTRAFICO)"))%>%
  nrow()

rin_hi_24 <- D_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, fecha_infraccion <= f_menos2d,
         presunta_motivacion_observacion =="RIÑAS")%>% nrow()

# 2023
ame_hi_23 <- H_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, fecha_infraccion <= f_hace_un_year,
         presunta_motivacion_observacion =="AMENAZA") %>% nrow()

traf_hi_23 <- H_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, fecha_infraccion <= f_hace_un_year,
         presunta_motivacion_observacion %in% c("TRAFICO INTERNACIONAL DE DROGA","TRAFICO INTERNOS DE DROGAS (MICROTRAFICO)"))%>%
  nrow()

rin_hi_23 <- H_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, fecha_infraccion <= f_hace_un_year,
         presunta_motivacion_observacion =="RIÑAS")%>% nrow()


# Crear la tabla consolidada
tabla_hi <- data.frame(
  "Presunta_motivacion_HI" = c("Amenazas", "Tráfico de drogas", "Riñas"),
  "n2023" = c(ame_hi_23, traf_hi_23, rin_hi_23),
  "n2024" = c(ame_hi_24, traf_hi_24, rin_hi_24)
)
		
### 4. Victimas de HI con antecedentes -------


antece_hi_24 <- D_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, fecha_infraccion <= f_menos2d) %>%
  group_by(antecedentes_siipne) %>% summarise(n=n())

antece_hi_23 <- H_HI_F %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, fecha_infraccion <= f_hace_un_year) %>%
  group_by(antecedentes_siipne) %>% summarise(n=n())



bd_hi_anteced <- data.frame(antece = c("SI","NO","SINDATO"), n_2023 = NA, n_2024 = NA)
antece_hi_24
antece_hi_23

# Actualizar las columnas n_2023 y n_2024 usando left_join y mutate para evitar conflictos
bd_hi_anteced <- bd_hi_anteced %>%
  left_join(antece_hi_23, by = c("antece" = "antecedentes_siipne")) %>%
  mutate(n_2023 = n) %>%
  select(-n) %>%  # Eliminar la columna 'n' para evitar conflicto
  left_join(antece_hi_24, by = c("antece" = "antecedentes_siipne")) %>%
  mutate(n_2024 = n) %>%
  select(-n)  # Eliminar la columna 'n' nuevamente



### 5. Número de muertes por semana ISO ----


max_sem_iso_24 <- fechas_semanas_iso_2023_2025 %>%
  filter(fecha_fin <= f_menos2d, ano == f_y_today) %>%
  summarise(max_sem = max(semana_iso)) %>%
  pull(max_sem)


# Comparar ambas semanas para encontrar la última semana completa común
ult_sem_completa <- max(max_sem_iso_24)


# Para el año 2023
n_hi_semana_23 <- H_HI_F %>%
  mutate(semana_iso = as.integer(isoweek(fecha_infraccion)),
         anio_iso = as.integer(isoyear(fecha_infraccion))
  ) %>% 
  filter(fecha_infraccion >= f_inicio_y_ant, semana_iso <= ult_sem_completa, anio_iso == f_y_ant) %>%
  group_by(semana_iso) %>% 
  summarise(n_2023 = n(),min23=min(fecha_infraccion),max23=max(fecha_infraccion))

# Para el año 2024
n_hi_semana_24 <- D_HI_F %>%
  mutate(semana_iso = as.integer(isoweek(fecha_infraccion)),
         anio_iso = as.integer(isoyear(fecha_infraccion))
  ) %>% 
  filter(fecha_infraccion >= f_inicio_y_hoy, semana_iso <= ult_sem_completa, anio_iso == f_y_today) %>%
  group_by(semana_iso) %>% 
  summarise(n_2024 = n(),min24=min(fecha_infraccion),max24=max(fecha_infraccion))

# Mostrar los resultados como data.frames
hi_semanas_iso_caso <- n_hi_semana_23 %>% left_join(n_hi_semana_24,by ="semana_iso" )

hi_semanas_iso_caso <- hi_semanas_iso_caso %>% select(semana_iso,n_2023,n_2024,everything()) %>%
  mutate(semana_iso=paste0("Sem.",semana_iso),"Año 2023"=n_2023,"Año 2024"=n_2024)

### 6. Tabla de contribución -----------


bd_hi_contrib <- bd_hi_subzonas
sum_hi_23 <- bd_hi_contrib$n_2023 %>% sum()
sum_hi_24 <- bd_hi_contrib$n_2024 %>% sum()

bd_hi_contrib$contrib23 <- bd_hi_contrib$n_2023/sum_hi_23
bd_hi_contrib$contrib24 <- bd_hi_contrib$n_2024/sum_hi_24

bd_hi_contrib <- bd_hi_contrib %>% arrange(desc(contrib24))
  
### ENSAMBLE DE UN PRODUCTO FINAL-----

df_HIDIA_FECHAS <- data.frame(
  "HI_DIA_24" = HI_DIA_24,
  "HI_DIA_23" = HI_DIA_23,
  "Fecha_Fin_2024" = f_menos2d,
  "Fecha_Fin_2023" = f_hace_un_year,
  "Fecha_ayer" = f_hoy-1
)


# EXPORTACIÓN A EXCEL -----------------------------------------------------


# Función para escribir el dataframe en una hoja específica
escribir_en_hoja <- function(ruta_archivo, data, nombre_hoja) {
  # Cargar el archivo existente
  wb <- loadWorkbook(ruta_archivo)
  
  # Si la hoja existe, sobrescribirla; si no, crear una nueva
  if (nombre_hoja %in% names(wb)) {
    removeWorksheet(wb, nombre_hoja)
  }
  
  # Escribir el data frame en la hoja especificada
  addWorksheet(wb, nombre_hoja)
  writeData(wb, sheet = nombre_hoja, x = data)
  
  # Guardar los cambios en el archivo
  saveWorkbook(wb, file = ruta_archivo, overwrite = TRUE)
}


# Ruta del archivo Excel
HOJA_PLANTILLA <- "HI/PLANTILLA/Reporte_homicidios_Plantilla_V2.xlsx"

# Lista de data frames y nombres de hojas
lista_dfs <- list(
  bd_hi_subzonas = bd_hi_subzonas,
  df_HIDIA_FECHAS = df_HIDIA_FECHAS,
  tabla_hi = tabla_hi,
  bd_hi_anteced = bd_hi_anteced,
  hi_semanas_iso_caso = hi_semanas_iso_caso,
  bd_hi_contrib = bd_hi_contrib
)

# Exportar cada data frame a una hoja específica
for (nombre_df in names(lista_dfs)) {
  escribir_en_hoja(HOJA_PLANTILLA, lista_dfs[[nombre_df]], nombre_df)
}
  
# Preparar listas para la exportación de semanas

lista_BD_DIARIA<- list(
  
  D_HI_F=D_HI_F
)
for (nombre_df in names(lista_BD_DIARIA)) {
  escribir_en_hoja(HOJA_PLANTILLA, lista_BD_DIARIA[[nombre_df]], nombre_df)
}







  
  