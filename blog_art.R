
library(readr)
library(dplyr)

url_csv <- 'https://raw.githubusercontent.com/d4tagirl/uruguayan_names/master/uy_names/nombre_nacim_x_anio_sexo.csv'
nombres_orig <- read_csv(url(url_csv), 
                         col_names = FALSE,
                         col_types = cols(X2 = col_factor(levels = c("F", "M")))) %>%
  rename(año = X1,
         sexo  = X2,
         nombre = X3,
         frec = X4) 



library(knitr)
knitr::kable(head(nombres_orig[c(1:5),]), format = "html", 
             table.attr = "style='width:30%;'")

library(stringi)

nombres <- nombres_orig %>% 
  mutate(nombre = stri_trans_general(trimws(nombre), id = "latin-ascii"),
         nombre = stri_replace_all_regex(nombre, pattern = "[0-9-,?<+.'´]+", "")) %>% 
  filter(nombre != "",
         año != 2012) %>%
  group_by(nombre, año) %>%
  summarise(frec = sum(frec)) %>% 
  ungroup()

library(tidyr)

nombres_año <- nombres  %>%
  complete(nombre, año, fill = list(frec = 0)) %>% 
  group_by(nombre) %>% 
  mutate(total_nombre = sum(frec)) %>% 
  ungroup() %>% 
  mutate(porc_nombre = frec / total_nombre) %>% 
  arrange(desc(total_nombre))

library(DT)
datatable(nombres_año[c(1:100),], rownames = FALSE,
          options = list(pageLength = 5))

library(ggplot2)
library(viridis)
nombres_año %>% 
  select(nombre, total_nombre) %>% 
  distinct(nombre, total_nombre) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(nombre, total_nombre), total_nombre, fill = reorder(nombre, total_nombre))) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE)

nombres_año %>% 
  filter(año > 1980) %>% 
  select(nombre, frec) %>% 
  group_by(nombre) %>% 
  mutate(total_nombre = sum(frec)) %>% 
  ungroup() %>% 
  distinct(nombre, total_nombre) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(nombre, total_nombre), total_nombre, fill = reorder(nombre, total_nombre))) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE)

nombres_unicos <- nombres_año %>% 
  select(nombre, total_nombre) %>% 
  distinct(nombre, total_nombre) %>% 
  filter(total_nombre == 1)

library(DT)
datatable(nombres_unicos, rownames = FALSE,
          options = list(pageLength = 5))

library(here)

nombres_año %>% 
  select(nombre, total_nombre) %>% 
  distinct(nombre, total_nombre) %>% 
  filter(total_nombre > 200) %>% 
  top_n(-20) %>% 
  ggplot(aes(reorder(nombre, total_nombre), total_nombre, fill = reorder(nombre, total_nombre))) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Nombres menos frecuentes") +
  labs(subtitle = "que se registraron más de 200 veces")

ggsave(filename = here("", "nombres_menos_usados.png"), dpi = 100)


library(magick) 

background <- image_read(here("", "nombres_menos_usados.png"))

gif <- image_read(here("", "confused_2.gif"))

frames <- lapply(gif, function(frame) {
  image_composite(background, frame, offset = "+250+130")
  })

animation <- image_animate(image_join(frames))

image_write(animation, "nombres_menos_usados.gif")


