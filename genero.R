library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

# cargar datos
sinim <- arrow::read_parquet("datos/sinim_genero_2019-2023.parquet")

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
    mutate(cambio = (porcentaje/lead(porcentaje))-1) |>
    # que primera medición sea cero
    mutate(cambio = if_else(año == min(año) & is.na(cambio), 0, cambio)) |> 
    # categórica
    mutate(tipo = if_else(cambio > 0, "positivo", "negativo")) |> 
    ungroup()
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


sinim_b |> 
  filter(variable == "Nº de mujeres no profesionales a contrata (sin título profesional)")


# variación inter-anual ----

sinim_b |> 
  filter(nombre_comuna %in% c("La Florida", "Puente Alto", "Macul")) |>
  # filter(nombre_comuna %in% c("La Florida")) |> 
  filter(variable %in% c("Número de hombres planta",
                         "Número de mujeres planta")) |> 
  calcular_cambio() |> 
  mutate(max = max(cambio)*0.02) |> 
  ggplot() +
  aes(x = año, y = cambio,
      group = nombre_comuna,
      fill = tipo, color = tipo) +
  geom_col(color = NA, width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(data = ~filter(.x, cambio != 0),
            aes(label = percent(cambio, 0.1),
                vjust = ifelse(cambio > 0, 0, 1),
                y = ifelse(cambio > 0, cambio+0.005, cambio-0.005)),
            size = 3, color = "black") +
  scale_y_continuous(limits = c(-0.1, 0.1), 
                     labels = label_percent()) +
  scale_fill_manual(values = c("negativo" = "red3", "positivo" = "grey60")) +
  guides(fill = guide_none()) +
  facet_wrap(~nombre_comuna, ncol = 1, axes = "all") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(), axis.ticks.x = element_blank(),
        panel.spacing.y = unit(8, "mm"),
        axis.title = element_blank())



# lista de variables ----
sinim |> 
  distinct(variable, variable_id, area, subarea) |> 
  print(n=Inf)


# filtrar región ----
# filtrar datos de una región y una variable

sinim |> distinct(nombre_region)

datos_region <- sinim |> 
  # filter(codigo_region == "02") |>
  filter(nombre_region == "Tarapacá") |> 
  # filter(variable_id == 1231) |> 
  filter(variable == "Porcentaje de mujeres funcionarias municipales") |> 
  filter(!is.na(valor)) |> 
  mutate(tipo = if_else(valor < 50, "Bajo 50%", "Sobre 50%"))

# comunas a incluir, por si son demasiadas
comunas_top <- datos_region |> 
  group_by(nombre_comuna) |> 
  summarize(n = sum(valor)) |> 
  slice_max(n, n = 7) |> 
  pull(nombre_comuna)




# porcentajes ----
datos_region |> 
  filter(nombre_comuna %in% comunas_top) |> 
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



# tendencia ----
# regresión lineal

datos_region |> 
  filter(nombre_comuna %in% comunas_top) |> 
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
  stat_smooth(method = "lm", se = T, geom = "ribbon", linewidth = 0, fullrange = T, alpha = 0.08) + # márgenes sin borde
  # texto
  ggrepel::geom_text_repel(data = datos_region |> group_by(nombre_comuna) |> slice_max(año),
                           aes(label = stringr::str_wrap(nombre_comuna, 12),
                               x = 2023.1), 
                           nudge_x = 0.1, min.segment.length = 1,
                           direction = "y", fontface = "bold", xlim = c(0, Inf),
                           size = 3.1, hjust = 0, lineheight = 0.8) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  labs(title = datos_region |> pull(variable) |> unique(),
       subtitle = datos_region |> pull(nombre_region) |> unique(),
       caption = "Fuente: datos.sinim.gov.cl") +
  # scale_x_continuous(expand = expansion(c(0, 0.25))) +
  scale_y_continuous(expand = expansion(c(0.2, 0.2)),
                     labels = ~paste0(" ", .x, "%"),
                     # limits = c(0, NA)
  ) +
  labs(y = datos_region |> pull(variable) |> unique(), x = NULL) +
  guides(color = guide_none(),
         fill = guide_none()) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit(c(4, 20, 2, 2), "mm"))

