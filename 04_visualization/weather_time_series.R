library(data.table)
library(geobr)
library(ggplot2)
library(dplyr)
library(zoo)
library(geobr)


# Definir o diretório de trabalho
setwd("C:/Users/User/OneDrive - Fundacao Getulio Vargas - FGV/Green innovation")

# Carregar os dados
weather <- fread("./Output/weather data/final_weather_data_1940_2020.csv")
mun <- read_municipality(code_muni = "all", year = 2020)
setDT(mun)

weather <- weather[,.(municipality_code,year,media_anual_max_temp,media_anual_precipitation,cont_shock_temperature,cont_shock_precipitation)]

# 0) MERGE ----


weather <- merge(weather,mun, by.x = "municipality_code",by.y = "code_muni" )


# 1) DATA PREPARATION ----
# We'll assume your dataframe is called 'weather' and has the columns:
# municipality_code, year, media_anual_max_temp, media_anual_precipitation

# a) Aggregate by year for TEMPERATURE
weather_temp <- weather %>%
  group_by(year) %>%
  summarize(mean_temp = mean(media_anual_max_temp, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year)

# b) Calculate a 10-year rolling mean
# (If you prefer a different window, change 'k = 10'.)
weather_temp <- weather_temp %>%
  mutate(roll_10yr = rollmean(mean_temp, k = 10, fill = NA, align = "right"))

# c) Aggregate by year for PRECIPITATION
weather_precip <- weather %>%
  group_by(year) %>%
  summarize(mean_precip = mean(media_anual_precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year)

# d) Calculate a 10-year rolling mean for precipitation
weather_precip <- weather_precip %>%
  mutate(roll_10yr = rollmean(mean_precip, k = 5, fill = NA, align = "right"))


# 2) PLOT FOR TEMPERATURE ----
p_temp <- ggplot(weather_temp, aes(x = year)) +
  # 12-month average (in your case, yearly data)
  geom_line(aes(y = mean_temp, color = "12-month average"), size = 0.7) +
  # 10-year moving average
  geom_line(aes(y = roll_10yr, color = "10-year average"), size = 1.2) +
  scale_color_manual(
    name = "",
    values = c("12-month average" = "blue", "10-year average" = "red")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Mean Temperature (°C)",
    caption = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Print the temperature plot
print(p_temp)

# Save the temperature plot
ggsave("./graphs/temperature_plot.png", plot = p_temp, width = 8, height = 6, dpi = 300)


# 3) PLOT FOR PRECIPITATION ----
p_precip <- ggplot(weather_precip[year > 1999], aes(x = year)) +
  # 12-month average (yearly data)
  geom_line(aes(y = mean_precip, color = "12-month average"), size = 0.7) +
  # 10-year moving average
  geom_line(aes(y = roll_10yr, color = "10-year average"), size = 1.2) +
  scale_color_manual(
    name = "",
    values = c("12-month average" = "blue", "10-year average" = "red")
  ) +
  labs(
    title = "",
    x = "Year",
    y = "Mean Precipitation (mm)",
    caption = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Print the precipitation plot
print(p_precip)

# Save the precipitation plot
ggsave("./graphs/precipitation_plot.png", plot = p_precip, width = 8, height = 6, dpi = 300)