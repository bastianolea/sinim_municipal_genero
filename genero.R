library(stringr)
library(tidyr)

sinim <- arrow::read_parquet("datos/sinim_genero_2019_2023.parquet")

sinim

sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)

sinim

sinim_b <- sinim |> 
select(variable, nombre_comuna, año, genero, valor) |> 
arrange(nombre_comuna, desc(año), variable)

calcular_cambio <- function(data) {
  data |> 
    group_by(nombre_comuna, año) |> 
    mutate(porcentaje = valor/sum(valor)) |> 
    filter(genero == "Mujeres") |> 
    drop_na(porcentaje) |> 
    group_by(nombre_comuna) |> 
    mutate(cambio = (porcentaje/lead(porcentaje))-1)
}

sinim_b |> 
  filter(variable %in% c("Número de hombres planta",
                         "Número de mujeres planta")) |> 
  calcular_cambio()

sinim_b |> 
  filter(variable %in% c("Número de hombres a contrata",
                         "Número de mujeres a contrata")) |> 
  calcular_cambio() |> 
  print(n=30)

sinim_b |> 
  filter(variable %in% c("Nº de hombres a honorarios (Subtítulo 21.03.000)",
                         "Nº de mujeres a honorarios (Subtítulo 21.03.000)")) |> 
  calcular_cambio()

sinim_b |> 
  filter(variable %in% c("Nº de mujeres profesionales a contrata",
                         "Nº de hombres profesionales a contrata")) |> 
  calcular_cambio()

sinim_b |> 
  filter(variable %in% c("Nº de mujeres no profesionales a contrata (sin título profesional)",
                         "Nº de hombres no profesionales a contrata (sin título profesional)")) |> 
  calcular_cambio()


datos_sinim |> 
  filter(variable == "Nº de mujeres no profesionales a contrata (sin título profesional)")
