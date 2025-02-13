library(dplyr)

source("funciones.R")

# cargar datos crudos
datos_sinim <- readr::read_rds("datos/datos_originales/sinim_scraping_2019-2023.rds")

# revisar variables
datos_sinim |> 
  distinct(variable, area, subarea)

# revisiar municipios
datos_sinim |> 
  distinct(municipio, municipio_name)

datos_sinim |> distinct(unit)  

# cargar códigos únicos territoriales
cut_comunas <- readr::read_csv2("datos/cut_comuna.csv") |> 
  select(ends_with("region"), ends_with("comuna"), -abreviatura_region)

# obtener lista con todos los municipios de sinim
municipios <- sinim_obtener_municipios()

# agregar códigos únicos territoriales
datos_sinim_2 <- datos_sinim |>
  # agregar coincidencia entre id de municipio y cut
  left_join(municipios |> select(id_municipio, codigo_comuna = idLegal),
            by = join_by(municipio == id_municipio)) |> 
  # agregar información a partir del cruce con cut
  left_join(cut_comunas,
          by = join_by(codigo_comuna))

# limpieza
datos_sinim_3 <- datos_sinim_2 |> 
  # sacar columnas irrelevantes
  select(-class_type, -col_info) |> 
  # reordenar columnas de municipio
  select(-municipio, -municipio_name) |> 
  relocate(ends_with("region"), ends_with("comuna"), contains("year"), .before = var_code) |> 
  # limpiar variables
  mutate(unit = str_trim(unit)) |> 
  # convertir cifras a numérico
  mutate(value = parse_number(value, locale = locale(decimal_mark = ",", grouping_mark = ".")))

# datos_sinim_3 |> 
#   filter(is.na(value2) & !is.na(value)) |> 
#   distinct(value)


# renombrar
datos_sinim_4 <- datos_sinim_3 |> 
  rename(año_id = sinim_year_code,
         año = user_year,
         variable_id = var_code,
         variable_desc = description,
         unidad = unit,
         valor = value)

# crear variables
datos_sinim_5 <- datos_sinim_4 |> 
  ungroup() |> 
  mutate(genero = case_when(str_detect(variable, "mujer|Mujer") ~ "Mujeres",
                            str_detect(variable, "hombre|Hombre") ~ "Hombres")) |> 
  
  mutate(genero = case_when(str_detect(variable, "total|Total") & !str_detect(variable, "Porcentaje.*sobre") ~ "Total",
                            .default = genero)) |> 
  select(-unidad) |> 
  mutate(medida = case_when(str_detect(variable, "porcentaje|Porcentaje") ~ "%",
                            str_detect(variable, "Número|Nº|número") ~ "N",
                            .default = "N")) |>
  mutate(ubicacion = case_when(str_detect(variable, "municipal") ~ "Municipio",
                               .default = "Público")) |>
  mutate(calificacion = case_when(str_detect(variable, "no profesional") ~ "No profesional",
                                  str_detect(variable, "profesional") ~ "Profesional",
                                  .default = "Otros")) |>
  mutate(tipo = case_when(str_detect(variable, "directivo") ~ "Directivos/as",
                          str_detect(variable, "contrata") ~ "Contrata",
                          str_detect(variable, "planta") ~ "Planta",
                          str_detect(variable, "honorario") ~ "Honorarios",
                          str_detect(variable, "comunitario") ~ "Honorarios",
                          str_detect(variable, "funcionari") ~ "Funcionarios/as",)) |> 
  mutate(programa = case_when(str_detect(variable, "comunitario") ~ "Comunitario",
                              .default = "Otros")) |> 
  # versión neutra de la variable, para poder seleccionarla y desagregar por la variable género
  mutate(variable_neutra = str_replace(variable, "(M|m)ujeres|(H|h)ombres|(M|m)ujer|(H|h)ombre", "personas"))

# datos_sinim_5 |> distinct(variable, genero, variable_desc, area, subarea) |> 
#   print(n=Inf)

# datos_sinim_5 |> 
#   mutate(variable_neutra = str_replace(variable, "mujeres|hombres|mujer|hombre", "personas")) |> 
#   distinct(variable, genero, variable_desc, area, subarea)


# guardar ----
datos_sinim_5 |> arrow::write_parquet("datos/sinim_genero_2019-2023.parquet")
datos_sinim_5 |> readr::write_csv("datos/sinim_genero_2019-2023.csv")
datos_sinim_5 |> writexl::write_xlsx("datos/sinim_genero_2019-2023.xlsx")
