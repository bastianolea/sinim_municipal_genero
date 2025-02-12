library(dplyr)
library(ggplot2)

# cargar datos ----
sinim <- arrow::read_parquet("datos/sinim_genero_2019-2023.parquet")

# lista de variables ----
sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)
