sinim <- arrow::read_parquet("datos/sinim_genero_2020_2023.parquet")


sinim |> 
  count(variable, area, subarea)


sinim |> 
  filter(año == 2023) |> 
  filter(variable_id == 1231) |> 
  filter(municipio == "LA FLORIDA")
  
