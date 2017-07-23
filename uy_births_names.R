library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(purrr)
library(broom)
library(stringi)
library(viridis)

raw <- read_csv("data/nombre_nacim_x_anio_sexo.csv", col_names = FALSE) %>%
  rename(year = X1,
         sex  = X2,
         name = X3,
         freq = X4) %>% 
  mutate(name = stri_trans_general(trimws(name), "latin-ascii"),
         name = stri_replace_all_regex(name, "[0-9-,?<+.'´]+", "")) %>% 
  filter(name != "",
         year != 2012) %>%
  group_by(name, year) %>%
  summarise(freq = sum(freq)) %>% 
  ungroup()

names_per_year <- raw %>%
  group_by(year) %>%
  summarize(year_total = sum(freq))

names_year_counts <- raw  %>%
  complete(name, year, fill = list(freq = 0)) %>% 
  group_by(name) %>% 
  mutate(name_total = sum(freq)) %>% 
  ungroup() %>% 
  left_join(names_per_year, by = "year") %>% 
  mutate(percent_year = freq / year_total,
         percent_name = freq / name_total) %>% 
  arrange(desc(name_total))

names_year_counts %>% 
  select(name, name_total) %>% 
  distinct(name, name_total) %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(name, name_total), name_total, fill = reorder(name, name_total))) +
  geom_col() +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE)


ggplotly(selected_name() %>%
           ggplot(aes(year, freq, colour = name,
                      text = paste('año: ', year,
                                   '<br /> cantidad : ', freq))) +
           geom_line(group = 1) +
           geom_point(size = 0.1) +
           ggtitle("Cantidad de nacidos con el nombre por año \n ") +
           theme_minimal() +
           theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                 axis.line = element_line(colour = "grey"), legend.title = element_blank(),
                 legend.position = 'bottom',
                 panel.grid.major = element_blank(), panel.border = element_blank()),
         tooltip = 'text')

  
# # ......................
# #   Old and new names

slopes <- names_year_counts %>%
  filter(name_total > 800) %>% 
  group_by(name) %>%
  nest(-name) %>%
  mutate(models = map(data, ~ lm(percent_name ~ year, .))) %>%
  unnest(map(models, tidy)) %>%
  filter(term == "year") %>%
  arrange(desc(estimate))

head(slopes, 12) %>% bind_rows(tail(slopes, 12)) %>%
  ggplot(aes(reorder(name, estimate), estimate, fill = reorder(name, estimate))) +
  geom_col(aes(fill = estimate > 0), show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis(discrete=TRUE)



# # ..................
# #   Names spiking

spiking <- names_year_counts %>%
  filter(name_total > 800) %>% 
  group_by(name) %>%
  mutate(weird = abs(percent_name - median(percent_name)) > 4*sd(percent_name)) %>%
  ungroup() %>%
  filter(weird == TRUE) %>%
  select(name) %>% 
  unique()

spiking_names <- spiking %>%
  left_join(names_year_counts)

ggplot(spiking_names, aes(year, freq, color = name)) +
  geom_line(group = 1) +
  facet_wrap( ~ reorder(name, desc(freq))) +
  theme_minimal() 


# david's approach

library(splines)

# Fit a cubic spline to each shape
spline_predictions <- names_year_counts %>%
  filter(name_total > 800) %>% 
  nest(-name) %>%
  mutate(model = map(data, ~ glm(percent_name ~ ns(year, 4), ., family = "binomial"))) %>%
  unnest(map2(model, data, augment, type.predict = "response"))

# Find the terms with the highest peak / average ratio
peak_per_year <- spline_predictions %>%
  group_by(name) %>%
  mutate(average = mean(.fitted)) %>%
  top_n(1, .fitted) %>%
  ungroup() %>%
  mutate(ratio = .fitted / average) %>%
  # filter(year != min(year), year != max(year)) %>%
  top_n(10, ratio) %>% 
  select(name) %>% 
  inner_join(names_year_counts) %>% 
  ggplot(aes(year, freq, color = name)) +
  geom_line(group = 1) +
  facet_wrap( ~ reorder(name, desc(freq))) +
  theme_minimal() 
peak_per_year


# early peaks 

early_peaks <- spline_predictions %>%
  group_by(name) %>%
  mutate(average = mean(.fitted)) %>%
  top_n(1, .fitted) %>%
  ungroup() %>%
  mutate(ratio = .fitted / average) %>%
  filter(year < 1955) %>% 
  top_n(10, ratio) %>% 
  select(name) %>% 
  inner_join(names_year_counts) %>% 
  ggplot(aes(year, freq, color = name)) +
  geom_line(group = 1) +
  facet_wrap( ~ reorder(name, desc(freq))) +
  theme_minimal()
early_peaks

# late peaks 

late_peaks <- spline_predictions %>%
  group_by(name) %>%
  mutate(average = mean(.fitted)) %>%
  top_n(1, .fitted) %>%
  ungroup() %>%
  mutate(ratio = .fitted / average) %>%
  filter(year > 2010) %>% 
  top_n(10, ratio) %>% 
  select(name) %>% 
  inner_join(names_year_counts) %>% 
  ggplot(aes(year, freq, color = name)) +
  geom_line(group = 1) +
  facet_wrap( ~ reorder(name, desc(freq))) +
  theme_minimal()
late_peaks

# 
# ggplotly(
#   names_year_counts %>%
#     filter(name == "CATRIEL") %>%
#     ggplot(aes(x = year)) +
#     # geom_line(aes(y = percent_year)) +
#     geom_line(aes(y = freq, color = "red"))
# )




# .............................
# quantity of names per year

variety <- raw %>%
  group_by(year) %>%
  summarize(year_total = n()) %>% 
  ggplot(aes(year, year_total)) +
  geom_col() +
  geom_smooth()





