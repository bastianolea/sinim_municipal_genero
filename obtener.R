library(tidyverse)

# funciones para scraping
source("funciones.R")

# obtener lista de variables
variables <- sinim_obtener_variables()

# ver todas las variables disponibles
# variables |> distinct(area, subarea) |> print(n=Inf)

# definir variables a obtener
variables_id <- variables |> 
  filter(area == "08.  GENERO") |> 
  pull(code)

# obtener lista con todos los municipios
municipios <- sinim_obtener_municipios()

# definir municipios
municipios_id <- municipios |> 
  pull(id_municipio)

# resumen previo
message(paste("obteniendo", length(variables_id), "variables para", length(municipios_id), "municipios"))

# realizar solicitud y obtener datos
datos_sinim <- sinim_obtener_datos(
  years            = c(2019:2023),
  var_codes        = variables_id,
  municipios       = municipios_id,
  parallel_workers = 8
)
# se demora aprox 1.5 horas

# guardar datos crudos
datos_sinim |> readr::write_rds("datos/datos_originales/sinim_scraping_2019-2023.rds", compress = "gz")