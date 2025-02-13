library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(gghighlight)

source("funciones.R")


# cargar datos
sinim <- arrow::read_parquet("datos/sinim_genero_2019-2023.parquet")

sinim

sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)

sinim

sinim_b <- sinim |> 
select(variable, nombre_comuna, año, genero, valor, variable_neutra) |> 
arrange(nombre_comuna, desc(año), variable)


# revisar datos ----
sinim_b |> 
  # filter(variable %in% c("Número de hombres planta",
  #                        "Número de mujeres planta")) |> 
  calcular_cambio()

sinim_b |> 
  filter(variable %in% c("Número de hombres a contrata",
                         "Número de mujeres a contrata")) |> 
  calcular_cambio() |> 
  print(n=30)

# sinim_b |> 
#   filter(variable %in% c("Nº de hombres a honorarios (Subtítulo 21.03.000)",
#                          "Nº de mujeres a honorarios (Subtítulo 21.03.000)")) |> 
#   calcular_cambio()
# 
# sinim_b |> 
#   filter(variable %in% c("Nº de mujeres profesionales a contrata",
#                          "Nº de hombres profesionales a contrata")) |> 
#   calcular_cambio()
# 
# sinim_b |> 
#   filter(variable %in% c("Nº de mujeres no profesionales a contrata (sin título profesional)",
#                          "Nº de hombres no profesionales a contrata (sin título profesional)")) |> 
#   calcular_cambio()
# 
# 
# sinim_b |> 
#   filter(variable == "Nº de mujeres no profesionales a contrata (sin título profesional)")



# g variación inter-anual ----
sinim_b |> 
  filter(nombre_comuna %in% c("La Florida", "Puente Alto", "Macul")) |>
  # filter(variable %in% c("Número de hombres planta",
  #                        "Número de mujeres planta")) |> 
  filter(variable_neutra %in% c("Número de personas planta")) |> 
  calcular_cambio() |> 
  grafico_variacion()

sinim_b |> 
  filter(nombre_comuna %in% c("La Florida", "Puente Alto", "Macul")) |>
  filter(variable_neutra %in% c("Número de personas a contrata")) |> 
  calcular_cambio() |> 
  grafico_variacion()


# lista de variables ----
sinim |> 
  distinct(variable, genero, variable_desc, area, subarea) |> 
  # slice(1) |> pull(variable_desc)
  print(n=Inf)


# filtrar región ----
# filtrar datos de una región y una variable

sinim |> distinct(nombre_region)



datos_region <- sinim |> 
  filter(nombre_region == "Ñuble") |> 
  filter(variable == "Porcentaje de mujeres funcionarias municipales") |> 
  filter(!is.na(valor)) |> 
  mutate(tipo = if_else(valor < 50, "Bajo 50%", "Sobre 50%"))


# comunas a incluir, por si son demasiadas
comunas_top <- datos_region |> 
  summarize(n = sum(valor), .by = nombre_comuna) |> 
  slice_max(n, n = 7) |> pull(nombre_comuna)


# g porcentajes ----
datos_region |> 
  filter(nombre_comuna %in% comunas_top) |> 
  grafico_porcentajes() +
  gghighlight(nombre_comuna == "Coelemu", 
              use_direct_label = F)



# g tendencia ----
# regresión lineal

datos_region |> 
  filter(nombre_comuna %in% comunas_top) |> 
  grafico_tendencias() +
  gghighlight(nombre_comuna == "Coelemu", 
              use_direct_label = F)

