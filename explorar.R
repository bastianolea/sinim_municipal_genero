library(dplyr)
library(ggplot2)

# cargar datos ----
sinim <- arrow::read_parquet("datos/sinim_genero_2020_2023.parquet")
cut_comunas <- readr::read_csv2("datos/cut_comuna.csv")

sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)

sinim |> 
  filter(año == 2023) |> 
  filter(variable_id == 1231) |> 
  filter(municipio == "LA FLORIDA")
  

# filtrar ----
# filtrar datos de una región y una variable
datos_region <- sinim |> 
  rename(codigo_comuna = cut_comuna) |> 
  left_join(cut_comunas, 
            by = join_by(codigo_comuna)) |> 
  filter(codigo_region == "01") |>
  filter(variable_id == 1231) |> 
  filter(!is.na(valor)) |> 
  mutate(tipo = if_else(valor < 50, "Bajo 50%", "Sobre 50%"))

# gráfico ----
datos_region |> 
  ggplot() + 
  aes(x = año, y = valor, 
      # color = tipo,
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
  ggrepel::geom_text_repel(data = datos_region |> group_by(nombre_comuna) |> slice_max(año),
            aes(label = stringr::str_wrap(nombre_comuna, 12)), 
            nudge_x = 0.1, min.segment.length = 1,
            direction = "y", fontface = "bold",
            size = 3.1, hjust = 0, lineheight = 0.8) +
  theme_minimal() +
  labs(title = datos_region |> pull(variable) |> unique(),
       subtitle = datos_region |> pull(nombre_region) |> unique(),
       caption = "Fuente: datos.sinim.gov.cl") +
  scale_x_continuous(expand = expansion(c(0, 0.25))) +
  scale_y_continuous(expand = expansion(c(0.2, 0.2)),
                     labels = ~paste0(" ", .x, "%"),
                     # limits = c(0, NA)
                     ) +
  labs(y = datos_region |> pull(variable) |> unique(), x = NULL) +
  guides(color = guide_none()) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())
  
