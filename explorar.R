library(dplyr)
library(ggplot2)

# cargar datos ----
sinim <- arrow::read_parquet("datos/sinim_genero_2019-2023.parquet")

# lista de variables ----
sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)


sinim
"Porcentaje de mujeres funcionarias municipales"
"Número de mujeres planta" 
"Número de mujeres a contrata"    
"Mujeres pertenecientes a escalafón directivo y profesional (planta y contrata)" 
"Nº de mujeres a honorarios (Subtítulo 21.03.000)" 