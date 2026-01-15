# ------------------------------------------------------------
# 1) INSTALL AND LOAD PACKAGES (if not already installed)
# ------------------------------------------------------------
# install.packages(c("dplyr", "ggplot2", "geobr", "sf", "cowplot", "rmapshaper"))

library(dplyr)
library(ggplot2)
library(geobr)       # For Brazilian geospatial data
library(sf)          # Spatial data handling
library(cowplot)     # Combine multiple ggplots
library(rmapshaper)  # To simplify polygons and avoid timeouts

# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar os dados
weather <- fread("./Output/weather data/final_weather_data_1940_2020.csv")

weather <- weather[,.(municipality_code,year,media_anual_max_temp,media_anual_precipitation,cont_shock_temperature,cont_shock_precipitation)]

# ------------------------------------------------------------
# 2) READ BRAZILIAN MUNICIPALITY SHAPEFILE
# ------------------------------------------------------------
sf_brazil <- read_municipality(year = 2020)

# ------------------------------------------------------------
# 3) SIMPLIFY THE SHAPEFILE TO REDUCE RENDERING COMPLEXITY
# ------------------------------------------------------------
sf_brazil_simplified <- ms_simplify(sf_brazil, keep = 0.05)

# ------------------------------------------------------------
# 4) SUMMARIZE WEATHER DATA (TEMPERATURE) FOR TWO PERIODS
# ------------------------------------------------------------
temp_2000_2010 <- weather %>%
  filter(year >= 2000, year <= 2010) %>%
  group_by(municipality_code) %>%
  summarize(mean_temp_2000_2010 = mean(cont_shock_precipitation, na.rm = TRUE)) %>%
  ungroup()

temp_2011_2018 <- weather %>%
  filter(year >= 2011, year <= 2020) %>%
  group_by(municipality_code) %>%
  summarize(mean_temp_2011_2018 = mean(cont_shock_precipitation, na.rm = TRUE)) %>%
  ungroup()

# ------------------------------------------------------------
# 5) JOIN SUMMARIES WITH THE SIMPLIFIED SHAPEFILE
# ------------------------------------------------------------
sf_brazil_2000_2010 <- sf_brazil_simplified %>%
  left_join(temp_2000_2010, by = c("code_muni" = "municipality_code"))

sf_brazil_2011_2018 <- sf_brazil_simplified %>%
  left_join(temp_2011_2018, by = c("code_muni" = "municipality_code"))

# ------------------------------------------------------------
# 6) CREATE TWO MAPS SIDE BY SIDE (REMOVE LAT/LONG AXES)
# ------------------------------------------------------------
map_2000_2010 <- ggplot() +
  geom_sf(data = sf_brazil_2000_2010,
          aes(fill = mean_temp_2000_2010),
          color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    na.value = "grey80"
  ) +
  labs(
    title = "(a) 2000-2010",
    fill  = "Avg Precip (mm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank()
  )

map_2011_2018 <- ggplot() +
  geom_sf(data = sf_brazil_2011_2018,
          aes(fill = mean_temp_2011_2018),
          color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    na.value = "grey80"
  ) +
  labs(
    title = "(b) 2011-2020",
    fill  = "Avg Precip (mm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank()
  )

# Combine both maps horizontally
two_maps <- plot_grid(map_2000_2010, map_2011_2018, ncol = 2)

# ------------------------------------------------------------
# 7) ADD AN OVERALL TITLE
# ------------------------------------------------------------
map_title <- ggdraw() + 
  draw_label("",
             fontface = 'bold',
             x = 0.5, hjust = 0.5)

final_plot <- plot_grid(
  map_title, 
  two_maps, 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)



# ------------------------------------------------------------
# 8) SAVE THE FINAL FIGURE TO A FILE
# ------------------------------------------------------------
ggsave("./graphs/final_precipitation_maps.png", final_plot, width = 12, height = 6, dpi = 300)



# ------------------------------------------------------------
# 4) SUMMARIZE WEATHER DATA (TEMPERATURE) FOR TWO PERIODS
# ------------------------------------------------------------
temp_2000_2010 <- weather %>%
  filter(year >= 2000, year <= 2010) %>%
  group_by(municipality_code) %>%
  summarize(mean_temp_2000_2010 = mean(cont_shock_temperature, na.rm = TRUE)) %>%
  ungroup()

temp_2011_2020 <- weather %>%
  filter(year >= 2011, year <= 2020) %>%
  group_by(municipality_code) %>%
  summarize(mean_temp_2011_2020 = mean(cont_shock_temperature, na.rm = TRUE)) %>%
  ungroup()

# ------------------------------------------------------------
# 5) JOIN SUMMARIES WITH THE SIMPLIFIED SHAPEFILE
# ------------------------------------------------------------
sf_brazil_2000_2010 <- sf_brazil_simplified %>%
  left_join(temp_2000_2010, by = c("code_muni" = "municipality_code"))

sf_brazil_2011_2020 <- sf_brazil_simplified %>%
  left_join(temp_2011_2020, by = c("code_muni" = "municipality_code"))

# ------------------------------------------------------------
# 6) DETERMINE THE DATA RANGE ACROSS BOTH PERIODS (OPTIONAL)
# ------------------------------------------------------------
all_values <- c(temp_2000_2010$mean_temp_2000_2010, 
                temp_2011_2020$mean_temp_2011_2020)
range(all_values, na.rm = TRUE)
# For example, you might see something like: -1.5 to 2.7
common_limits <- c(-1.5, 2.7)  # Adjust if needed

# ------------------------------------------------------------
# 7) CREATE TWO MAPS WITH THE SAME COLOR LIMITS (NO LAT/LONG)
# ------------------------------------------------------------
map_2000_2010 <- ggplot() +
  geom_sf(data = sf_brazil_2000_2010,
          aes(fill = mean_temp_2000_2010),
          color = NA) +
  scale_fill_distiller(
    palette   = "Reds",
    direction = 1,
    na.value  = "grey80",
    limits    = common_limits
  ) +
  labs(
    title = "(a) 2000-2010",
    fill  = "Avg Temp (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

map_2011_2020 <- ggplot() +
  geom_sf(data = sf_brazil_2011_2020,
          aes(fill = mean_temp_2011_2020),
          color = NA) +
  scale_fill_distiller(
    palette   = "Reds",
    direction = 1,
    na.value  = "grey80",
    limits    = common_limits
  ) +
  labs(
    title = "(b) 2011-2020",
    fill  = "Avg Temp (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

# Combine both maps horizontally
two_maps <- plot_grid(map_2000_2010, map_2011_2020, ncol = 2)

# ------------------------------------------------------------
# 8) ADD AN OVERALL TITLE AND SAVE
# ------------------------------------------------------------
map_title <- ggdraw() + 
  draw_label("",
             fontface = 'bold',
             x = 0.5, hjust = 0.5)

final_plot <- plot_grid(
  map_title, 
  two_maps, 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

ggsave("./graphs/final_temperature_maps.png", final_plot, width = 12, height = 6, dpi = 300)

