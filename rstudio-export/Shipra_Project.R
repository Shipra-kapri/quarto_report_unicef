install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

# Data
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

# Data Transform
data_join <- full_join(unicef_indicator_2, unicef_metadata, by = c("country","time_period" = "year"))

# World Map
map_world <- map_data("world")

# Sorting top 30 country with highest obs_value
top_30 <- unicef_indicator_2 %>%
  group_by(country) %>%
  summarise(obs_value = sum(obs_value)) %>%
  top_n(30, obs_value)

# Creating map for top 30
map_data_join_top30 <- full_join(top_30, map_world, by = c("country" = "region"))

ggplot(map_data_join_top30) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  labs(title = "Top 30 Countries with Highest level of Diarrhoea") +
  scale_fill_continuous(low = "blue", high = "red", na.value = "lightpink", name = "Obs_Value") +
  geom_polygon()

options(scipen = 999)

# Timeseries plot

# Data
unicef_metadata <- read.csv("unicef_metadata.csv")

# Filter data for desired Countries 
filter_data <- unicef_metadata %>%
  filter(country %in% c( "Algeria", "Bangladesh","India", "Nepal","Thailand", "Zambia"))

ggplot(filter_data, aes(x = year, y = GDP.per.capita..constant.2015.US.., group = country, color = "darkred")) +
  geom_line() +
  facet_wrap(~ country, ncol = 3) +
  ggtitle("Time Series Chart of Observed Value for Country by Time Period") +
  xlab("Time Period 1960-2022") +
  ylab("GDP Per Capita (Constant 2015 US$)") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "darkgreen"),
    panel.background = element_rect(fill = "grey97")
  )



# bar chart Plot

# To clean data 
clean_data <- unicef_metadata %>%
  filter(!is.na(Life.expectancy.at.birth..total..years.) & Life.expectancy.at.birth..total..years. >= 0)

# Sorting top 10 countries with higest mean life expectancy
top_10 <- clean_data %>%
  group_by(country) %>%
  summarise(Life.expectancy.at.birth..total..years. = mean(Life.expectancy.at.birth..total..years.)) %>%
  top_n(10, Life.expectancy.at.birth..total..years.)

bar_chart <- ggplot(top_10, aes(x = reorder(country, Life.expectancy.at.birth..total..years.), y = Life.expectancy.at.birth..total..years.)) +
  geom_bar(stat = "identity", fill = "maroon") +
  coord_flip() +
  labs(title = "Top Countries with the Highest Mean Life Expectancy over Time",
       x = "Country",
       y = "Life Expectancy") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "darkgreen"),
    panel.background = element_rect(fill = "grey97")
  )
bar_chart


# Scatter Plot
# Data
data_join <- full_join(unicef_indicator_2, unicef_metadata, by = c("country","time_period" = "year"))

# To remove all NA data for obs_value and population along with its corresponding rows
valid_data <- data_join %>%
  filter(!is.na(obs_value) & obs_value >= 0)

valid_data <- valid_data %>%
  filter(!is.na(Population..total) & Population..total >= 0)

# Scatter Plot
ggplot(valid_data, aes(x = obs_value, y = Population..total, color = sex)) +
  geom_point() +
  ggtitle("SScatter Plot for Total Population vs Diarrhoea Treatment") +
  xlab("Diarrhoea Treatment") +
  ylab("Total Population") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "darkred"),
    panel.background = element_rect(fill = "lavender"))




