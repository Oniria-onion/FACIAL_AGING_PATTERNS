# Este es el código para reproducir el artículo
# PATRONES DE ENVEJECIMIENTO FACIAL EN FOTOGRAFÍAS DE HOMBRES Y 
# MUJERES MEXICANOS PARA LA ELABORACIÓN DE RETRATOS DE PROGRESIÓN DE EDAD 
# Hernández Vargas, Oniria Guadalupe y Farrera, Arodi

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# Leyendo los datos ####
mujeres <- read.csv("datos_crudos_mujeres.csv", header = T)
hombres <- read.csv("datos_crudos_hombres.csv", header = T)

# Función para calcular la moda
get_moda <- function(v) {
  v <- v[!is.na(v)]
  # Si el vector se queda vacío (todos eran NA), devolvemos NA
  if (length(v) == 0) return(NA)
  
  uniqs <- unique(v)
  uniqs[which.max(tabulate(match(v, uniqs)))]
}


# DIFERENCIAS POR SEXO ####
library(purrr)
library(tidyr)
library(gt)


# Tabla 2  ####
mujeres$sexo <- "Mujeres"
hombres$sexo <- "Hombres"
todos_difSexo <- rbind(mujeres, hombres)

# Cálculo diferencias estadísticas mann whitney
TABLA2_calculo <- todos_difSexo %>% 
  filter(!grupo_edad %in% c("50-60", "60-70", "70+") ) %>%
  group_by(marca, grupo_edad) %>% 
  nest() %>% 
  mutate(prueba = map(data, ~ wilcox.test(Intensidad ~ sexo,
                                          data = .x,
                                          exact = FALSE)),
         p_value = map_dbl(prueba, ~ .x$p.value),
         statistic = map_dbl(prueba, ~ unname(.x$statistic))
         ) %>% 
  select(marca, grupo_edad, p_value, statistic) %>% 
  ungroup()

# Formato diferencias estadísticas
TABLA2_formato <- TABLA2_calculo %>% mutate(
  p_valor = ifelse(p_value < 0.001, "<0.001", 
                    sprintf("%.3f", p_value)),
  W = round(statistic, 1)
  ) %>%
  select(marca, grupo_edad, W, p_valor) %>%
  pivot_wider(
    names_from = grupo_edad,
    values_from = c(W, p_valor)
  )

TABLA2 <- TABLA2_formato %>% gt() %>% cols_label(marca = "Marca") %>%
  cols_align(align = "center", columns = -marca) %>%
  tab_style(style = cell_text(whitespace = "pre-wrap", size = "small"),
            locations = cells_body(columns = everything() ) )

TABLA2

gtsave(TABLA2, "wilcoxon2.rtf")


# GRÁFICAS DE INTENSIDAD POR SEXO Y GRUPO DE EDAD ####
# Escalas de rango 0 a 5 ####
mujeres_escala0a5 <- mujeres %>% filter(!marca %in%  
                                           c("Hipercromía del anillo orbitario", 
                                           "Flacidez de la piel de la mandíbula", 
                                           "Surco infraorbitario"))
mujeres_escala0a5$grupo_edad <- as.factor(mujeres_escala0a5$grupo_edad)
mujeres_escala0a5$Intensidad <- as.factor(mujeres_escala0a5$Intensidad)

hombres_escala0a5 <- hombres %>% filter(!marca %in%  
                                           c("Hipercromía del anillo orbitario", 
                                             "Flacidez de la piel de la mandíbula", 
                                             "Surco infraorbitario"))
hombres_escala0a5$grupo_edad <- as.factor(hombres_escala0a5$grupo_edad)
hombres_escala0a5$Intensidad <- as.factor(hombres_escala0a5$Intensidad)


# Cálculo de la moda 
mujeres_moda <- mujeres_escala0a5 %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
mujeres_moda$sexo <- "Mujeres"

hombres_moda <- hombres_escala0a5 %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
hombres_moda$sexo <- "Hombres"

# Unión de las bases de datos para graficar
todos_modas <- rbind(mujeres_moda, hombres_moda)
todos_modas$sexo <- factor(todos_modas$sexo, levels = c("Mujeres", "Hombres"))
todos_modas <- todos_modas %>%
  mutate(moda = factor(moda, levels = 0:5, ordered = TRUE))
todos_modas <- todos_modas %>% filter(grupo_edad != "70")

# Ordenar las marcas para graficar
todos_modas$marca <- factor(todos_modas$marca, levels = c(
  "Pliegues del cuello",
  "Flacidez de la piel de la mandíbula", 
  "Pliegue de la barbilla", 
  "Líneas de quelion",
  "Líneas de la comisura de la boca", 
  "Líneas periorales",
  "Pliegues nasolabiales", 
  "Pliegues de las mejillas", 
  "Líneas periauriculares", 
  "Hipercromía del anillo orbitario", 
  "Surco infraorbitario", 
  "Líneas periorbitarias", 
  "Líneas glabelares", 
  "Líneas horizontales de la frente" ))

# Figura 2 ####
todos_modas %>%
  ggplot(aes(x = grupo_edad, y = marca, size = moda, color = moda)) +
  geom_point(alpha = 0.8) + facet_wrap(~sexo) + 
  scale_size_discrete(name = "Moda", range = c(2, 7)) +
  scale_color_viridis_d(name = "Moda", option = "viridis", direction = -1) +
  theme_bw() + labs(x = NULL, y = NULL ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,
                               hjust = 1, size = 14, 
                               family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 14, family = "Arial")
  )

# Escalas de rango diferente ####
# OJERAS
mujeres_ojeras <- mujeres %>% filter(marca == "Hipercromía del anillo orbitario")
mujeres_ojeras$grupo_edad <- as.factor(mujeres_ojeras$grupo_edad)
mujeres_ojeras$Intensidad <- as.factor(mujeres_ojeras$Intensidad)

hombres_ojeras <- hombres %>% filter(marca == "Hipercromía del anillo orbitario")
hombres_ojeras$grupo_edad <- as.factor(hombres_ojeras$grupo_edad)
hombres_ojeras$Intensidad <- as.factor(hombres_ojeras$Intensidad)

# Calcular moda
mujeres_ojeras_moda <- mujeres_ojeras %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
mujeres_ojeras_moda <- mujeres_ojeras_moda %>% filter(grupo_edad != "70")
mujeres_ojeras_moda$sexo <- "Mujeres"

hombres_ojeras_moda <- hombres_ojeras %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
hombres_ojeras_moda <- hombres_ojeras_moda %>% filter(grupo_edad != "70")
hombres_ojeras_moda$sexo <- "Hombres"

todos_ojeras <- rbind(mujeres_ojeras_moda, hombres_ojeras_moda)
todos_ojeras$sexo <- factor(todos_ojeras$sexo, levels = c("Mujeres", "Hombres"))

# Graficar
o <- todos_ojeras %>%
  ggplot(aes(x = grupo_edad, y = marca, size = moda, color = moda)) +
  geom_point(alpha = 0.8) + facet_wrap(~sexo) + 
  scale_size_discrete(name = "Moda", range = c(2, 7)) +
  scale_color_viridis_d(name = "Moda", option = "viridis", direction = -1) +
  theme_bw() + labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,
                               hjust = 1, size = 14, 
                               family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 14, family = "Arial")
  )

# FLACIDEZ
mujeres_flacidez <- mujeres %>% filter(marca == "Flacidez de la piel de la mandíbula")
mujeres_flacidez$grupo_edad <- as.factor(mujeres_flacidez$grupo_edad)
mujeres_flacidez$Intensidad <- as.factor(mujeres_flacidez$Intensidad)

hombres_flacidez <- hombres %>% filter(marca == "Flacidez de la piel de la mandíbula")
hombres_flacidez$grupo_edad <- as.factor(hombres_flacidez$grupo_edad)
hombres_flacidez$Intensidad <- as.factor(hombres_flacidez$Intensidad)

# Calcular moda
mujeres_flacidez_moda <- mujeres_flacidez %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
mujeres_flacidez_moda <- mujeres_flacidez_moda %>% filter(grupo_edad != "70")
mujeres_flacidez_moda$sexo <- "Mujeres"

hombres_flacidez_moda <- hombres_flacidez %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
hombres_flacidez_moda <- hombres_flacidez_moda %>% filter(grupo_edad != "70")
hombres_flacidez_moda$sexo <- "Hombres"

todos_flacidez <- rbind(mujeres_flacidez_moda, hombres_flacidez_moda)
todos_flacidez$sexo <- factor(todos_flacidez$sexo, levels = c("Mujeres", "Hombres"))

# Graficar
f <- todos_flacidez %>%
  ggplot(aes(x = grupo_edad, y = marca, size = moda, color = moda)) +
  geom_point(alpha = 0.8) + facet_wrap(~sexo) + 
  scale_size_discrete(name = "Moda", range = c(2, 7)) +
  scale_color_viridis_d(name = "Moda", option = "viridis", direction = -1) +
  theme_bw() + labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,
                               hjust = 1, size = 14, 
                               family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 14, family = "Arial")
  )


# SURCO
mujeres_surco <- mujeres %>% filter(marca == "Surco infraorbitario")
mujeres_surco$grupo_edad <- as.factor(mujeres_surco$grupo_edad)
mujeres_surco$Intensidad <- as.factor(mujeres_surco$Intensidad)

hombres_surco <- hombres %>% filter(marca == "Surco infraorbitario")
hombres_surco$grupo_edad <- as.factor(hombres_surco$grupo_edad)
hombres_surco$Intensidad <- as.factor(hombres_surco$Intensidad)

# Calcular moda
mujeres_surco_moda <- mujeres_surco %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
mujeres_surco_moda <- mujeres_surco_moda %>% filter(grupo_edad != "70")
mujeres_surco_moda$sexo <- "Mujeres"

hombres_surco_moda <- hombres_surco %>% group_by(grupo_edad, marca) %>%
  summarise(moda = get_moda(Intensidad))
hombres_surco_moda <- hombres_surco_moda %>% filter(grupo_edad != "70")
hombres_surco_moda$sexo <- "Hombres"

todos_surco <- rbind(mujeres_surco_moda, hombres_surco_moda)
todos_surco$sexo <- factor(todos_surco$sexo, levels = c("Mujeres", "Hombres"))

# Graficar
s <- todos_surco %>%
  ggplot(aes(x = grupo_edad, y = marca, size = moda, color = moda)) +
  geom_point(alpha = 0.8) + facet_wrap(~sexo) + 
  scale_size_discrete(name = "Moda", range = c(2, 7)) +
  scale_color_viridis_d(name = "Moda", option = "viridis", direction = -1) +
  theme_bw() + labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,
                               hjust = 1, size = 14, 
                               family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 14, family = "Arial")
  )


# FIGURA 3 ####
s / o / f + plot_layout(guides = 'collect') + plot_layout(axis_titles = "collect")




# ENVEJECIMIENTO INDIVIDUAL  ####
library(tidyr)
mujeres_individual <- mujeres[,-c(3,4)] %>% 
  pivot_wider(names_from = marca, values_from = Intensidad)
# seleccionar las marcas
mujeres_individual <- mujeres_individual[, c(1:6, 11)]

patron_mujeres <- mujeres_individual %>% group_by(grupo_edad) %>%
  unite("patron", `Líneas horizontales de la frente`:`Pliegues nasolabiales`, 
        sep = "-", remove = FALSE) %>%
  count(patron, name = "frecuencia") %>% as.data.frame()


hombres_individual <- hombres[,-c(3,4)] %>% 
  pivot_wider(names_from = marca, values_from = Intensidad)
# seleccionar las marcas
hombres_individual <- hombres_individual[, c(1:6, 11)]

patron_hombres <- hombres_individual %>% group_by(grupo_edad) %>%
  unite("patron", `Líneas horizontales de la frente`:`Pliegues nasolabiales`, 
        sep = "-", remove = FALSE) %>%
  count(patron, name = "frecuencia") %>% as.data.frame()


# Tablas 3 Y 4 ####  
patron_mujeres[order(patron_mujeres$grupo_edad, 
                     -patron_mujeres$frecuencia), ]

patron_hombres[order(patron_hombres$grupo_edad, 
                     -patron_hombres$frecuencia), ]
