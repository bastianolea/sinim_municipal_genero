library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(gghighlight)
library(shinyWidgets)
library(gt)

source("funciones.R")

# cargar datos
sinim <- arrow::read_parquet("datos/sinim_genero_2019-2023.parquet")

regiones <- sinim |> distinct(nombre_region) |> pull() |> na.omit()

variables_porcentaje <- sinim |> distinct(variable) |> pull() |> str_subset("Porcentaje")

variables_variacion <- sinim |> distinct(variable_neutra) |> pull() |> 
  str_subset("Nº", negate = T) |> 
  str_subset("Subtítulo", negate = T)



ui <- page_fluid(
  tags$head(tags$style('.card{overflow: visible !important;}'),
            tags$style('.card-body{overflow: visible !important;}')),
  
  h1("Sinim"),
  
  card(
    selectInput("region",
                label = "Regiones",
                choices = regiones)
  ),
  
  # parte 1 ----
  card(min_height = "600px",
       
       card_body(
         layout_columns(
           selectInput("v_porcentaje",
                     "Variables",
                     choices = variables_porcentaje),
           sliderInput("comunas_n",
                       "Comunas",
                       min = 2, max = 15,
                       value = 7)
         ),
         
         layout_columns(
           plotOutput("g_porcentajes"),
           plotOutput("g_tendencia")
         ),
         
         selectInput("comunas_destacar",
                     "Destacar",
                     choices = NULL),
         
         gt_output("t_porcentajes")
       )
  ),
  
  # parte 2 ----
  card(min_height = "600px",
       selectInput("v_variacion",
                   "Variables",
                   choices = variables_variacion),
       
       pickerInput("comunas_variacion",
                   "Comunas",
                   choices = "La Florida",
                   multiple = TRUE),
       
       div(style = css(min_width = "700px"),
           plotOutput("g_variacion")
       )
       
  )
  
  
)

server <- function(input, output, session) {
  
  
  # parte 1 ----
  datos_filtro_region <- reactive({
    sinim |> 
      # filter(nombre_region == "Ñuble") |> 
      filter(nombre_region == input$region) 
  })
  
  datos_region <- reactive({
    datos_filtro_region() |> 
      # filter(variable == "Porcentaje de mujeres funcionarias municipales") |> 
      filter(variable == input$v_porcentaje) |> 
      filter(!is.na(valor)) |> 
      mutate(tipo = if_else(valor < 50, "Bajo 50%", "Sobre 50%"))
  })
  
  # comunas a incluir, por si son demasiadas
  comunas_top <- reactive({
    datos_region() |> 
      summarize(n = sum(valor), .by = nombre_comuna) |> 
      slice_max(n, n = input$comunas_n) |> pull(nombre_comuna)
  })
  
  observe(
    updateSelectInput(session,
                      "comunas_destacar",
                      choices = c("Ninguna", comunas_top())
    )
  )
  
  output$g_porcentajes <- renderPlot({
    # browser()
    plot <- datos_region() |> 
      filter(nombre_comuna %in% comunas_top()) |> 
      grafico_porcentajes()
    
    if (input$comunas_destacar != "Ninguna") {
      plot <- plot +
        gghighlight(nombre_comuna == input$comunas_destacar,
                    use_direct_label = F)
    }
    return(plot)
  })
  
  output$t_porcentajes <- render_gt({
    datos_region() |> 
      select(nombre_comuna, año, valor) |> 
      tidyr::pivot_wider(names_from = año, values_from = valor) |> 
      gt() |> 
      data_color(columns = where(is.numeric),
                 direction = "column",
                 palette = "Purples") |> 
      opt_table_lines("none") |> 
      cols_label(nombre_comuna = "Comuna")
  })
  
  
  output$g_tendencia <- renderPlot({
    plot <- datos_region() |> 
      filter(nombre_comuna %in% comunas_top()) |> 
      grafico_tendencias()
    
    if (input$comunas_destacar != "Ninguna") {
      plot <- plot +
        gghighlight(nombre_comuna == input$comunas_destacar,
                    use_direct_label = F)
    }
    return(plot)
  })
  
  
  # parte 2 ----
  
  comunas_region <- reactive({
    datos_filtro_region() |> 
      pull(nombre_comuna) |> 
      unique() |> 
      sort()
  })
  
  observe(
    updatePickerInput(session,
                      "comunas_variacion",
                      choices = comunas_region(),
                      selected = comunas_region()[1:4]
    )
  )
  
  datos_filtro_comunas <- reactive({
    datos_filtro_region() |> 
      # filter(nombre_comuna %in% c("La Florida", "Puente Alto", "Macul"))
      filter(nombre_comuna %in% input$comunas_variacion)
  })
  
  output$g_variacion <- renderPlot({
    req(length(input$comunas_variacion) >= 1)
    
    # browser()
    
    datos_filtro_comunas() |> 
      # filter(variable_neutra %in% c("Número de personas planta")) |> 
      filter(variable_neutra == input$v_variacion) |> 
      calcular_cambio() |> 
      select(nombre_comuna, año, valor, medida, porcentaje, cambio, tipo) |> 
      print(n=Inf)
    
    datos_filtro_comunas() |> 
      # filter(variable_neutra %in% c("Número de personas planta")) |> 
      filter(variable_neutra == input$v_variacion) |> 
      calcular_cambio() |>
      grafico_variacion(orientacion = "horizontal")
  })
  
}

shinyApp(ui, server)