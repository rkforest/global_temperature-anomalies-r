library(tidyverse)
library(ggthemes)
library(viridis)
library(paletteer)
library(patchwork)

climate_periods_end <- c(1930,1960,1990,2020)
climate_periods_start <- climate_periods_end - 30
climate_periods = paste(
  as.character(climate_periods_start),"-",
  as.character(climate_periods_end))
climate_periods

temperature_category_end <- c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5)
temperature_category_start <- c(-2.0, 0.0, 0.5, 1.0, 1.5, 2.0)
temperature_categories = paste(
  as.character(temperature_category_start),"to",
  as.character(temperature_category_end))
temperature_categories[1] <- " < 0"
temperature_categories[6] <- " > 2"
temperature_categories

csv_url = "https://data.giss.nasa.gov/gistemp/tabledata_v4/"
csv_file_names <- c("GLB.Ts+dSST.csv",
                    "NH.Ts+dSST.csv",
                    "SH.Ts+dSST.csv")

df_list <- list()
skip_header_recs <- c(1, 1, 1)

for (i in 1:length(csv_file_names)) {
  csv_file_path <- paste(csv_url, csv_file_names[i], sep = "")
  csv_save_path <- file.path("data", "raw", "tabular", csv_file_names[i])
  df <- read_csv(file=csv_file_path,
                 skip=skip_header_recs[i],
                 na = "***",
                 show_col_types = FALSE)
  df_list[[i]] <- df
}

global_raw_data <-df_list[[1]] 
northern_raw_data <-df_list[[2]] 
southern_raw_data <-df_list[[3]] 

glimpse(global_raw_data)
glimpse(northern_raw_data)
glimpse(southern_raw_data)

fn_tidy <- function(df, pivot_cols, pivot_name) {
  sel_cols <- c("Year", pivot_cols)
  dft <- df |> 
    select(all_of(sel_cols)) |> 
    pivot_longer(
      cols=all_of(pivot_cols), 
      names_to = pivot_name,
      values_to = "Anomaly",
      values_drop_na = TRUE)
  return(dft)
}

pivot_name <- "Month"
pivot_cols <- colnames(global_raw_data[,2:13]) 

global_tidy_data <- fn_tidy(global_raw_data, pivot_cols, pivot_name)
northern_tidy_data <- fn_tidy(northern_raw_data, pivot_cols, pivot_name)
southern_tidy_data <- fn_tidy(southern_raw_data, pivot_cols, pivot_name)

glimpse(global_tidy_data)
glimpse(northern_tidy_data)
glimpse(southern_tidy_data)

month_codes <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
seasons <- c("Winter","Spring","Summer","Autumn")

global_identifier <- "G"
global_label <- c("Global")
hemisphere_identifiers <- c("S","N")
hemisphere_labels <- c("Southern", "Northern")

latest_year <- global_tidy_data[[nrow(global_tidy_data),1]]
decade_start <- latest_year- 10

fn_add_climate_period <- function(df,
                                  climate_periods_end,
                                  climate_periods) 
{
  #  add 30 year climate period based on year
  #  change climate variable to factor and add labels
  dfc <- df |> 
    mutate(ClimatePeriod = case_when(
      Year <= climate_periods_end[1] ~ climate_periods[1],
      Year <= climate_periods_end[2] ~ climate_periods[2],
      Year <= climate_periods_end[3] ~ climate_periods[3],
      Year <= climate_periods_end[4] ~ climate_periods[4]))
  dfc$ClimatePeriod <- factor(dfc$ClimatePeriod,
                              levels=climate_periods,
                              labels=climate_periods)
  return(dfc)
} 

fn_add_temperature_category <- function(df,
                                        temperature_categories_end,
                                        temperature_categories) 
{
  #  add 30 year climate period based on year
  #  change climate variable to factor and add labels
  dftc <- df |> 
    mutate(TemperatureCategory = case_when(
      Anomaly <= temperature_category_end[1] ~ temperature_categories[1],
      Anomaly <= temperature_category_end[2] ~ temperature_categories[2],
      Anomaly <= temperature_category_end[3] ~ temperature_categories[3],
      Anomaly <= temperature_category_end[4] ~ temperature_categories[4],
      Anomaly <= temperature_category_end[5] ~ temperature_categories[5]))
  dftc$TemperatureCategory <- factor(dftc$TemperatureCategory,
                                     levels=temperature_categories,
                                     labels=temperature_categories)
  return(dftc)
} 

fn_add_season <- function(df) {
  dfs <- df |> 
    rename(Hemisphere=Identifier) |>
    mutate(Season = case_when(
      (Hemisphere == "N" &  Month %in% c("Dec","Jan","Feb")) ~ seasons[1],
      (Hemisphere == "N" &  Month %in% c("Mar","Apr","May")) ~ seasons[2],
      (Hemisphere == "N" &  Month %in% c("Jun","Jul","Aug")) ~ seasons[3],
      (Hemisphere == "N" &  Month %in% c("Sep","Oct","Nov")) ~ seasons[4],
      
      (Hemisphere == "S" &  Month %in% c("Jun","Jul","Aug")) ~ seasons[1],
      (Hemisphere == "S" &  Month %in% c("Sep","Oct","Nov")) ~ seasons[2],
      (Hemisphere == "S" &  Month %in% c("Dec","Jan","Feb")) ~ seasons[3],
      (Hemisphere == "S" &  Month %in% c("Mar","Apr","May")) ~ seasons[4]))
  
  dfs$Hemisphere <- factor(dfs$Hemisphere, 
                           levels=hemisphere_identifiers,
                           labels=hemisphere_labels)
  
  dfs$Season <- factor(dfs$Season,
                       levels=seasons)
  return(dfs)
}


fn_transform_monthly <- function(df, id) {
  dft <- df |> 
    filter(Year >= climate_periods_start[1]) |>
    mutate(Identifier = id) |>
    mutate(Identifier = factor(Identifier)) |> 
    mutate(Year = as.integer(Year)) |>
    mutate(Decade = as.integer(Year - (Year %% 10) + 10))
  dft$Month <- factor(dft$Month, levels=month_codes)
  dfcp <- fn_add_climate_period(dft, climate_periods_end, climate_periods) |>
    select(Identifier, ClimatePeriod, Decade, Year, Month, Anomaly) 
  dftc <- fn_add_temperature_category(dfcp, temperature_category_end, temperature_categories) |>
    select(Identifier, ClimatePeriod, Decade, Year, Month, Anomaly, TemperatureCategory) 
  return(dftc)
}

global_transformed_data <- fn_transform_monthly(global_tidy_data,id="G")
global_transformed_data$Identifier <- 
  factor(global_transformed_data$Identifier, 
         levels=global_identifier,
         labels=global_label)


glimpse(global_transformed_data)


northern_transformed_data <- fn_transform_monthly(northern_tidy_data,id="N")
southern_transformed_data <- fn_transform_monthly(southern_tidy_data,id="S")

hemisphere_transformed_data <- dplyr::bind_rows(northern_transformed_data, southern_transformed_data)
hemisphere_transformed_data<- fn_add_season(hemisphere_transformed_data)

glimpse(hemisphere_transformed_data)

plot_transparency <- 0.9

scale_option <- "D"
scale_direction <- -1
scale_beg <- 0.2
scale_end <- 1.0

palette <- "pals::coolwarm"
y_limits <- c(min(global_transformed_data$Anomaly),
              max(global_transformed_data$Anomaly))

p1 <- global_transformed_data |> 
  ggplot(aes(x=Year, y = Anomaly)) +
  geom_point(aes(color = Anomaly)) +
  ylim(y_limits ) +
  scale_color_paletteer_c(palette=palette, direction = 1) +
  geom_smooth(method = "lm") +
  labs(
    subtitle = "Average by Month",
    y = "°C",
    color = "°C")  +
  theme(axis.title.x=element_blank()) 

p2 <- global_transformed_data |> 
  group_by(Year) |> 
  summarize(avg_anomaly = mean(Anomaly)) |>
  ggplot(aes(x = Year, y = avg_anomaly)) +
  geom_line(aes(color = avg_anomaly),
            linewidth=0.8,
            show.legend = FALSE) +
  ylim(y_limits ) +
  scale_color_paletteer_c(palette=palette, direction = 1) + 
  labs(subtitle = "Average By Year",
       y = "°C") +
  theme(axis.title.x=element_blank())

p3 <- global_transformed_data |> 
  group_by(Decade) |> 
  summarize(avg_anomaly = mean(Anomaly)) |>
  ggplot(aes(x = Decade, y = avg_anomaly, color = avg_anomaly)) +
  geom_line(linewidth=0.8,show.legend = FALSE) +
  geom_point(size=1.5,show.legend = FALSE) +
  ylim(y_limits ) +
  scale_color_paletteer_c(palette=palette, direction = 1) + 
  labs(subtitle = "Average By Decade") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

plot_title <- paste("Global Temperature Anomalies 1881-" , latest_year)

layout <- "
AAAAA
AAAAA
BBBCC
"
time_series_plots_1 <- p1 + p2 + p3 +
  plot_layout(design = layout) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = plot_title)

time_series_plots_1

plot_title <- "Global Monthly Average Temperature Anomaly by Climate Period"

y_limits <- c(min(global_transformed_data$Anomaly),
              max(global_transformed_data$Anomaly))
p0 <- global_transformed_data |>
  filter(complete.cases(ClimatePeriod)) |> 
  ggplot(aes(x=Year, y=Anomaly, color=ClimatePeriod)) +
  geom_point(alpha=0.4) +
  ylim(y_limits ) +
  geom_smooth(method = "lm", size=2, alpha=1.0, se=FALSE) +
  labs(title = plot_title,
       y = "Anomaly °C",
       color = "Climate Period")  +
  theme(axis.title.x=element_blank()) +
  scale_x_continuous(breaks = c(1900, 1930, 1960, 1990, 2020)) +
  scale_color_colorblind()
p0


climate_plot_1 <- global_transformed_data |>
  filter(complete.cases(ClimatePeriod)) |> 
  ggplot(aes(x=Anomaly,fill=ClimatePeriod)) +
  geom_area(stat="bin",
            alpha=0.8,
            color="grey40") +
  labs(
    title = "Global Average Temperature Anomalies by Climate Period",
    x = "Temperature Anomaly (°C)",
    y = "Count",
    fill = "Climate Period")  +
  scale_fill_colorblind()
climate_plot_1

climate_plot_2 <- global_transformed_data |>
  filter(complete.cases(ClimatePeriod)) |> 
  ggplot(aes(x=Anomaly,fill=ClimatePeriod)) +
  geom_histogram(binwidth = 0.1,
                 alpha = 0.8,
                 color="grey30") +
  labs(
    title = "Global Average Temperature Anomalies by Climate Period",
    subtitle = "Binwidth 0.1 degrees",
    x = "Temperature Anomaly (°C)",
    y = "Count",
    fill = "Climate Period")  +
  scale_fill_colorblind()
climate_plot_2

climate_plot_3 <- global_transformed_data |> 
  filter(complete.cases(ClimatePeriod)) |> 
  ggplot(aes(x = ClimatePeriod, y = Anomaly, fill=ClimatePeriod)) +
  geom_boxplot(alpha = 0.65,
               show.legend = FALSE) +
  labs(
    title = "Global Average Temperature Anomalies by Climate Period",
    subtitle = "Median, Interquartile Range, and Outliers",
    x = "Climate Period",
    y = "Anomaly °C",
    fill = "Climate") +
  scale_fill_colorblind()
climate_plot_3

climate_plot_4 <- hemisphere_transformed_data  |> 
  filter(complete.cases(ClimatePeriod)) |> 
  group_by(ClimatePeriod, Hemisphere) |>
  summarize(avg_anomaly=mean(Anomaly)) |>
  ggplot(aes(x = ClimatePeriod, y = avg_anomaly, fill = ClimatePeriod)) +
  geom_bar(stat='identity',
           position='dodge',
           alpha = 0.8,
           color="black") +
  labs(title = "Average Temperature Anomaly By Hemisphere",
       y = "Anomaly °C",
       fill = "Climate Period") +
  facet_wrap(~Hemisphere, ncol=2) +
  scale_fill_colorblind() +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank())
climate_plot_4


climate_plot_5 <- hemisphere_transformed_data  |> 
  filter(complete.cases(ClimatePeriod)) |> 
  group_by(ClimatePeriod, Season) |>
  summarize(avg_anomaly=mean(Anomaly)) |>
  ggplot(aes(x = ClimatePeriod, y = avg_anomaly, fill = ClimatePeriod)) +
  geom_bar(stat='identity',
           position='dodge',
           alpha = 0.9,
           color="black") +
  facet_wrap(~Season, ncol=2) +
  labs(title = "Average Temperature Anomaly By Season",
       x = "Climate Period",
       y = "Anomaly °C",
       fill = "Climate Period")  +
  scale_fill_colorblind() +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank())
climate_plot_5

global_data_after_2020 <-
  global_transformed_data  |>
  filter(Year > 2020) 

hemisphere_data_after_2020 <-
  hemisphere_transformed_data  |>
  filter(Year > 2020) 

year_labels <- c("2021", "2022", "2023", "2024", "2025")


decade_plot_1 <- hemisphere_data_after_2020 |> 
  group_by(Hemisphere, Month) |>
  summarize(avg_anomaly=mean(Anomaly)) |>
  ggplot(aes(x = Hemisphere, y = avg_anomaly, fill = Month)) +
  geom_bar(stat='identity',
           position='dodge',
           alpha = plot_transparency,
           color="black") +
  labs(title = "Average Temperature Anomaly By Hemisphere and Month",
       subtitle = "2015-2025",
       x = "Hemisphere",
       y = "Anomaly °C") +
  scale_fill_viridis(option=scale_option,
                     begin = scale_beg,
                     end = scale_end,
                     direction = scale_direction,
                     discrete =TRUE)
decade_plot_1

scale_option <- "D"
scale_direction <- 1
scale_beg <- 0.0
scale_end <- 0.8

scale_option <- "D"
scale_direction <- 1
scale_beg <- 0.0
scale_end <- 0.7

decade_plot_2 <- hemisphere_data_after_2020 |> 
  group_by(Hemisphere, Season) |>
  summarize(avg_anomaly = mean(Anomaly)) |>
  ggplot(aes(x = Hemisphere, y = avg_anomaly, fill=Season)) +
  geom_bar(stat='identity',
           position='dodge',
           alpha = plot_transparency,
           color="black") +
  labs(title = "Average Temperature Anomaly By Hemisphere and Season",
       subtitle = "2015-2025",
       y = "Anomaly")  +
  scale_fill_colorblind() +
  theme(axis.title.x=element_blank())
decade_plot_2

plot_data <- global_transformed_data  |>
  filter(Year > 2020) |>
  filter(Anomaly >= 1.0)|>
  mutate(Year = factor(Year, levels = year_labels)) |> 
  count(Year, name = "Count", .drop = FALSE)
y_limits <- c(1,12)
decade_plot_3 <- plot_data |> 
  ggplot(aes(x = Year, y=Count, fill=Count)) +
  geom_col(color="black") +
  labs(title = "Count of Months per Year with Global Average Temperature > 1.0°C",
       subtitle = paste("2021 -", latest_year),
       x = "Year",
       y = "Count") +
  scale_color_colorblind() +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+
  coord_cartesian(ylim = c(0,12))
decade_plot_3

plot_data <- hemisphere_transformed_data  |>
  filter(Year > 2020) |>
  filter(Anomaly >= 1.5)|>
  mutate(Year = factor(Year, levels = year_labels)) |> 
  count(Year, name = "Count", .drop = FALSE)
y_limits <- c(1,12)
decade_plot_4 <- plot_data |> 
  ggplot(aes(x = Year, y=Count, fill=Count)) +
  geom_col(color="black") +
  labs(title = "Count of Months per Year with Hemisphere Average Temperature > 1.5°C",
       subtitle = "2015-2025",
       x = "Year",
       y = "Count") +
  scale_color_colorblind() +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  coord_cartesian(ylim = c(0,12))
decade_plot_4

plot_data <- hemisphere_transformed_data  |>
  filter(Year > 2020) |>
  filter(Anomaly >= 1.5) |>
  mutate(Year = factor(Year, levels = year_labels)) |> 
  count(Year, name = "Count", .drop = FALSE)
y_limits <- c(1,12)
decade_plot_5 <- plot_data |> 
  ggplot(aes(x = Year, y=Count, fill=Count)) +
  geom_col(color="black") +
  labs(title = "Count of Months per Year with Hemisphere Average Temperature > 1.5°C",
       subtitle = "2015-2025",
       x = "Year",
       y = "Count") +
  scale_color_colorblind() +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  coord_cartesian(ylim = c(0,12))
decade_plot_5

