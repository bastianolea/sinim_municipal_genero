



datos_sinim <- sinim_obtener_datos(
  years            = c(2021),
  var_codes        = c(657, 1253, 1231, 739, 4057, 4623),
  municipios       = c(183, 189),
  parallel_workers = 1
)


datos_sinim |> 
  filter(var_code == 657)

datos_sinim |> 
  filter(is.na(value))
