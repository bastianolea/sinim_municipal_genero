# https://gist.github.com/RRMaximiliano/ee65369679047baea7093af43d2434e6
# código por Rony Rodrigo Maximiliano Rodriguez Ramirez
# https://github.com/RRMaximiliano

library(httr)
library(jsonlite)
library(tidyverse)
library(rvest)
library(furrr)
library(progressr)

.session_cookie <- "PHPSESSID=abc123"

sinim_años <- function(years) {
  valid_years <- 2001:2023
  sinim_codes <- 2:24
  mapping     <- setNames(sinim_codes, valid_years)
  if (!all(years %in% valid_years)) {
    stop("Los años deben ser entre 2001 y 2023.")
  }
  unname(mapping[as.character(years)])
}

sinim_obtener_variables <- function() {
  url <- "https://datos.sinim.gov.cl/datos_municipales/obtener_datos_filtros.php"
  body <- list("dato_area[]" = "T", "dato_subarea[]" = "T")
  resp <- POST(
    url,
    body = body,
    encode = "form",
    add_headers(
      "Accept"           = "application/json, text/javascript, */*; q=0.01",
      "Content-Type"     = "application/x-www-form-urlencoded; charset=UTF-8",
      "X-Requested-With" = "XMLHttpRequest"
    ),
    set_cookies(.session_cookie)
  )
  stop_for_status(resp)
  
  raw_json <- content(resp, "text", encoding = "UTF-8")
  data_list <- fromJSON(raw_json)
  merged    <- Reduce(function(...) merge(..., all = TRUE), data_list)
  final     <- merged[, c(8, 10, 18, 2, 4, 5)]
  colnames(final) <- c("code", "variable", "description", "area", "subarea", "unit")
  final$code <- as.character(final$code)
  as_tibble(final)
}



sinim_obtener_municipio <- function(municipios, limit = 5000, region = "T") {
  if (missing(municipios) || length(municipios) < 1) {
    stop('Provide at least one municipality code or "T" for all.')
  }
  body_str <- paste0(
    "region[]=", region, "&",
    paste0("municipio[]=", municipios, collapse = "&"),
    # paste0("municipio[]=", c(180:700), collapse = "&"),
    "&limit=", limit,
    "&campo=id_legal&orden=ASC&pagina=1"
  )
  url <- "https://datos.sinim.gov.cl/datos_municipales/obtener_municipios.php"
  resp <- POST(
    url,
    body = body_str,
    encode = "form",
    add_headers(
      "accept" = "application/json, text/javascript, */*; q=0.01",
      "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "x-requested-with" = "XMLHttpRequest"
    ),
    set_cookies(.session_cookie)
  )
  stop_for_status(resp)
  
  raw_json <- content(resp, "text", encoding = "UTF-8")
  txt_data <- fromJSON(raw_json, simplifyDataFrame = TRUE)$textos
  txt_data
  nrow(txt_data)
  as_tibble(txt_data)
}

# obtiene todos los municipios de una veez
sinim_obtener_municipios <- function(municipios, limit = 5000, region = "T") {
  # if (missing(municipios) || length(municipios) < 1) {
  #   stop('Provide at least one municipality code or "T" for all.')
  # }
  body_str <- paste0(
    "region[]=", region, "&",
    # paste0("municipio[]=", municipios, collapse = "&"),
    paste0("municipio[]=", c(180:700), collapse = "&"),
    "&limit=", limit,
    "&campo=id_legal&orden=ASC&pagina=1"
  )
  url <- "https://datos.sinim.gov.cl/datos_municipales/obtener_municipios.php"
  resp <- POST(
    url,
    body = body_str,
    encode = "form",
    add_headers(
      "accept" = "application/json, text/javascript, */*; q=0.01",
      "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "x-requested-with" = "XMLHttpRequest"
    ),
    set_cookies(.session_cookie)
  )
  stop_for_status(resp)
  
  raw_json <- content(resp, "text", encoding = "UTF-8")
  txt_data <- fromJSON(raw_json, simplifyDataFrame = TRUE)$textos
  # txt_data
  # nrow(txt_data)
  as_tibble(txt_data)
}

.obtener_valores <- function(
    variable,
    sinim_year_code,
    municipio_id,
    municipio_name,
    idLegal,
    corrmon = FALSE
) {
  # browser()
  corrmon_flag <- ifelse(corrmon, "true", "false")
  url <- "https://datos.sinim.gov.cl/datos_municipales/obtener_valores.php"
  
  
  # algunas variables (las que empiezan con "N°", por ejemplo, variable = 657) no obtiene resultados porque hay que ponerle "tipo%5D=V",
  # pero como no sabemos a cuáles hay que ponerle, se intenta con "I" primero, y si falla, se intenta con "V"
  body_str <- paste0(
    "variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bid_periodo%5D=", sinim_year_code,
    "&variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bmtro_datos_tipo%5D=I",
    # "&variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bmtro_datos_tipo%5D=V",
    "&municipios%5B0%5D%5Bid_municipio%5D=", municipio_id,
    "&municipios%5B0%5D%5Bmunicipio%5D=", URLencode(municipio_name),
    "&municipios%5B0%5D%5BtipoCol%5D=par",
    "&municipios%5B0%5D%5BidLegal%5D=", idLegal,
    "&pagina=1",
    "&corrmon=", tolower(corrmon_flag)
  )
  
  # body_str <- "variables%5B1%5D%5B
  # 640
  # %5D%5B0%5D%5Bid_periodo%5D=
  # 23&
  # variables%5B1%5D%5B
  # 640
  # %5D%5B0%5D%5Bmtro_datos_tipo%5D=V&
  # municipios%5B0%5D%5Bid_municipio%5D=190&municipios%5B0%5D%5Bmunicipio%5D=ARICA&municipios%5B0%5D%5BtipoCol%5D=par&municipios%5B0%5D%5BidLegal%5D=15101&pagina=1&corrmon=false'"
  resp <- POST(url, body = body_str, encode = "form",
    add_headers("accept" = "application/json, text/javascript, */*; q=0.01",
      "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "x-requested-with" = "XMLHttpRequest"),
    set_cookies(.session_cookie))
  
  # resp
  # resp$status_code
  stop_for_status(resp)
  raw_txt <- content(resp, "text", encoding = "UTF-8")
  
  # si la respuesta es html en vez de json signfica que hubo un error; así que se intenta recuperar del error más común
  # reintentando el request con "tipo%5D=V"
  # si vuelve a fallar, se retorna NULL
  if (grepl("<html", raw_txt)) {
    warning("Se obtubo HTML en vez de JSON...")
    warning("Reintentando con variable V...")
    # return(NULL)
  
    # reintentar
    body_str <- paste0(
      "variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bid_periodo%5D=", sinim_year_code,
      # "&variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bmtro_datos_tipo%5D=I",
      "&variables%5B1%5D%5B", variable, "%5D%5B0%5D%5Bmtro_datos_tipo%5D=V",
      "&municipios%5B0%5D%5Bid_municipio%5D=", municipio_id,
      "&municipios%5B0%5D%5Bmunicipio%5D=", URLencode(municipio_name),
      "&municipios%5B0%5D%5BtipoCol%5D=par",
      "&municipios%5B0%5D%5BidLegal%5D=", idLegal,
      "&pagina=1",
      "&corrmon=", tolower(corrmon_flag))
    
    resp <- POST(url, body = body_str, encode = "form",
                 add_headers("accept" = "application/json, text/javascript, */*; q=0.01",
                             "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
                             "x-requested-with" = "XMLHttpRequest"),
                 set_cookies(.session_cookie))
    
    stop_for_status(resp)
    raw_txt <- content(resp, "text", encoding = "UTF-8")
    
    if (grepl("<html", raw_txt)) {
      return(NULL)
    }
  }
  
  tryCatch({
    data_parsed <- fromJSON(raw_txt, simplifyDataFrame = TRUE)
    as_tibble(data_parsed$textos)
  }, error = function(e) {
    warning("Error interpretando JSON.")
    NULL
  })
}

# Show a progress bar for row-wise retrieval using progressr
sinim_obtener_datos <- function(
    var_codes,
    years,
    municipios,
    corrmon          = FALSE,
    limit            = 5000,
    region           = "T",
    parallel_workers = 1,
    show_progress    = TRUE
) {
  # Activate progressr globally if show_progress is TRUE
  if (show_progress) {
    handlers(global = TRUE)
    handlers("progress")
  } else {
    handlers(global = FALSE)
  }
  
  plan(multisession, workers = parallel_workers)
  
  var_codes_chr <- as.character(var_codes)
  sinim_codes   <- sinim_años(years)
  var_labels    <- sinim_obtener_variables()
  muni_info     <- sinim_obtener_municipio(municipios, limit, region)
  
  if (nrow(muni_info) == 0) {
    stop("No se encontraron municipios coincidentes. Revisa los códigos de municipios, o usa 'T'.")
  }
  
  # browser()
  base_grid <- expand_grid(
    var_code        = var_codes_chr,
    sinim_year_code = sinim_codes,
    municipio       = muni_info$id_municipio
  ) %>%
    mutate(
      user_year = years[ match(sinim_year_code, sinim_años(years)) ]
    ) %>%
    rowwise() %>%
    mutate(
      municipio_name = muni_info$municipio[muni_info$id_municipio == municipio],
      idLegal        = muni_info$idLegal[muni_info$id_municipio == municipio]
    ) %>%
    ungroup()
  
  row_count <- nrow(base_grid)
  
  # if (show_progress) {
  with_progress({
    p <- progressor(steps = row_count)
    
    base_grid <- base_grid %>%
      mutate(
        data_values = future_pmap(
          list(var_code, sinim_year_code, municipio, municipio_name, idLegal),
          function(.var, .yearcode, .muni, .muni_name, .id_legal) {
            # Get the user-friendly year from the sinim_year_code
            user_year <- years[ match(.yearcode, sinim_años(years)) ]
            
            # Signal progress update exactly once per iteration
            p(sprintf("Obteniendo var = %s, year = %s, muni = %s", .var, user_year, .muni))
            
            # Custom debug message for timing
            start_time <- Sys.time()
            # browser()
            df <- tryCatch({
              .obtener_valores(
                variable         = .var,
                sinim_year_code  = .yearcode,
                municipio_id     = .muni,
                municipio_name   = .muni_name,
                idLegal          = .id_legal,
                corrmon          = corrmon
              )
            }, error = function(e) {
              warning(paste("Error obteniendo var = ", .var, " year = ", user_year, " muni = ", .muni))
              NULL
            })
            end_time <- Sys.time()
            elapsed_time <- end_time - start_time
            Sys.sleep(elapsed_time*10) #espera
            
            # Print custom debug message
            # message(sprintf("(%.3fs) Obteniendo var = %s, year = %s, muni = %s", elapsed_time, .var, user_year, .muni))
            
            if (!is.null(df)) {
              df |> 
                mutate(valor = as.character(valor)) |> 
                mutate(
                  code_var        = .var,
                  code_year       = .yearcode,
                  code_municipio  = .muni
                )
            } else {
              tibble(
                valor           = NA_character_,
                col             = NA_character_,
                classtype       = NA_character_,
                municipio       = NA_character_,
                code_var        = .var,
                code_year       = .yearcode,
                code_municipio  = .muni
              )
            }
          }
        )
      )
    
    # Explicitly disable the progressor after all steps are completed
    p(amount = 0, type = "finish")
  })
  # }
  
  final_df <- base_grid %>%
    unnest(data_values, names_sep = "_") %>%
    left_join(
      var_labels %>% mutate(code = as.character(code)),
      by = c("var_code" = "code")
    ) %>%
    rename(
      value      = data_values_valor,
      col_info   = data_values_col,
      class_type = data_values_classtype
    ) %>%
    select(
      var_code,
      variable,
      description,
      area,
      subarea,
      unit,
      sinim_year_code,
      user_year,
      municipio,
      municipio_name,
      value,
      col_info,
      class_type
    )
  
  final_df
}



calcular_cambio <- function(data) {
  data |> 
    arrange(nombre_comuna, desc(año)) |> 
    group_by(nombre_comuna, año) |> 
    mutate(porcentaje = valor/sum(valor)) |> 
    filter(genero == "Mujeres") |> 
    drop_na(porcentaje) |> 
    group_by(nombre_comuna) |> 
    # mutate(cambio = (porcentaje/lead(porcentaje))-1) |>
    mutate(cambio = case_when(medida == "%" ~ (valor/lead(valor))-1,
                              .default = (porcentaje/lead(porcentaje))-1)) |> 
    # que primera medición sea cero
    mutate(cambio = if_else(año == min(año) & is.na(cambio), 0, cambio)) |> 
    # categórica
    mutate(tipo = if_else(cambio > 0, "positivo", "negativo")) |> 
    ungroup()
}


grafico_variacion <- function(data, orientacion = "vertical") {
  plot <- data |> 
    mutate(max = max(cambio)*0.02) |> 
    ggplot() +
    aes(x = año, y = cambio,
        group = nombre_comuna,
        fill = tipo, color = tipo) +
    geom_col(color = NA, width = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_text(#data = ~filter(.x, cambio != 0),
      aes(label = percent(cambio, 0.1),
          vjust = ifelse(cambio > 0, 0, 1),
          y = case_when(cambio == 0 ~ 0.02,
                        cambio > 0 ~ cambio+0.005, 
                        cambio < 0 ~ cambio-0.005)),
      size = 3, color = "black") +
    scale_y_continuous(#limits = c(-0.1, 0.1), 
      # oob = scales::oob_squish,
      labels = label_percent()) +
    scale_fill_manual(values = c("negativo" = "red3", "positivo" = "grey60")) +
    guides(fill = guide_none()) +
    theme_classic() +
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold"),
          panel.grid.major.y = element_line(), axis.ticks.x = element_blank(),
          axis.title = element_blank())
  
  if (orientacion == "vertical") {
    plot <- plot +
      facet_wrap(~nombre_comuna, ncol = 1, axes = "all") +
      theme(panel.spacing.y = unit(8, "mm"))
  } else if (orientacion == "horizontal") {
    plot <- plot +
      facet_wrap(~nombre_comuna, nrow = 1, axes = "all") +
      theme(panel.spacing.x = unit(8, "mm"))
  }
  return(plot)
}



grafico_porcentajes <- function(data) {
  data |> 
    group_by(nombre_comuna) |> 
    mutate(nombre_comuna_max = ifelse(año == max(año), nombre_comuna, NA)) |> 
    ungroup() |> 
    ggplot() + 
    aes(x = año, y = valor, 
        color = nombre_comuna,
        group = nombre_comuna) +
    annotate(geom = "rect", 
             xmin = 2020-0.1, xmax = 2023+0, 
             ymin = 50, ymax = -Inf, 
             fill = "red3", alpha = 0.06) +
    geom_hline(yintercept = 50, linetype = "dashed", 
               color = "red3", alpha = .5, linewidth = 1) +
    geom_line(linewidth = 1.2, alpha = 0.8) + 
    geom_point(size = 3, alpha = 0.8) +
    ggrepel::geom_text_repel(aes(label = stringr::str_wrap(nombre_comuna_max, 12)),
                             nudge_x = 0.1, min.segment.length = 1,
                             direction = "y", fontface = "bold", xlim = c(0, Inf),
                             size = 3.1, hjust = 0, lineheight = 0.8) +
    theme_minimal() +
    coord_cartesian(clip = "off") +
    labs(title = data |> pull(variable) |> unique(),
         subtitle = data |> pull(nombre_region) |> unique(),
         caption = "Fuente: datos.sinim.gov.cl") +
    # scale_x_continuous(expand = expansion(c(0, 0.25))) +
    scale_y_continuous(expand = expansion(c(0.2, 0.2)),
                       labels = ~paste0(" ", .x, "%")) +
    labs(y = data |> pull(variable) |> unique(), x = NULL) +
    guides(color = guide_none()) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    theme(plot.margin = unit(c(4, 20, 2, 2), "mm"))
}



grafico_tendencias <- function(data) {
  data |> 
    group_by(nombre_comuna) |> 
    mutate(nombre_comuna_max = ifelse(año == max(año), nombre_comuna, NA)) |> 
    ungroup() |> 
    ggplot() + 
    aes(x = año, y = valor, 
        # color = tipo,
        color = nombre_comuna,
        fill = nombre_comuna,
        group = nombre_comuna) +
    # cuadro rojo
    # annotate(geom = "rect", xmin = min(datos_region$año)-0.1, xmax = max(datos_region$año)+0, 
    #          ymin = 50, ymax = -Inf, fill = "red3", alpha = 0.06) +
    # línea al 50%
    geom_hline(yintercept = 50, linetype = "dashed", 
               color = "red3", alpha = .5, linewidth = 0.4) +
    stat_smooth(method = "lm", se = F, linewidth = 1.2, fullrange = T, linetype = "dotted", alpha = 0.8) + # línea punteada
    stat_smooth(method = "lm", se = F, linewidth = 1.2, alpha = 0.8) + # línea sólida
    # stat_smooth(method = "lm", se = T, fullrange = T, geom = "ribbon", fill = NA, linetype = "dashed", alpha = 1) + # márgenes con borde
    stat_smooth(method = "lm", se = T, geom = "ribbon", linewidth = 0, fullrange = F, alpha = 0.09) + # márgenes sin borde
    # texto
    ggrepel::geom_text_repel(aes(label = stringr::str_wrap(nombre_comuna_max, 12),
                                 x = 2023.1), 
                             nudge_x = 0.1, min.segment.length = 1,
                             direction = "y", fontface = "bold", xlim = c(0, Inf),
                             size = 3.1, hjust = 0, lineheight = 0.8) +
    theme_minimal() +
    coord_cartesian(clip = "off") +
    labs(title = data |> pull(variable) |> unique(),
         subtitle = data |> pull(nombre_region) |> unique(),
         caption = "Fuente: datos.sinim.gov.cl") +
    scale_x_continuous(expand = expansion(c(0, 0))) +
    scale_y_continuous(expand = expansion(c(0.2, 0.2)),
                       labels = ~paste0(" ", .x, "%"),
                       # limits = c(0, NA)
    ) +
    labs(y = data |> pull(variable) |> unique(), x = NULL) +
    guides(color = guide_none(),
           fill = guide_none()) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    theme(plot.margin = unit(c(4, 20, 2, 2), "mm"))
}