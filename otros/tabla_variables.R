library(dplyr)

sinim <- arrow::read_parquet("datos/sinim_genero_2020_2023.parquet")

sinim |> 
  distinct(#area, subarea, 
           variable, medida, genero, calificacion, tipo) |> 
  knitr::kable(format = "markdown") |> # crear tabla en markdown
  clipr::write_clip() # copiar 



