setwd("../")

# Task1
library(dplyr)
library(ggplot2)
library(readr)
library(ggrepel)

# Read data
countryCodes <- read.csv("country_codes.csv") %>%
  rename('LOCATION' = 'CODE', 'country' = 'Country')

healthcareSpending <- read.csv("healthcare_spending.csv") %>%
  rename('spending_pct_gdp' = 'Value')

lifeExpectancy <- read.csv("life_expectancy.csv") %>%
  rename('life_expectancy' = 'Value')

# Filter and select data for 2018
healthcareSpending_2018 <- healthcareSpending %>%
  filter(TIME == 2018, SUBJECT == 'TOT', MEASURE == 'PC_GDP') %>%
  select(LOCATION, spending_pct_gdp)

lifeExpectancy_2018 <- lifeExpectancy %>%
  filter(TIME == 2018, SUBJECT == 'TOT') %>%
  select(LOCATION, life_expectancy)

# Merge data and filter complete cases
merged_df <- countryCodes %>%
  left_join(healthcareSpending_2018, by = 'LOCATION') %>%
  left_join(lifeExpectancy_2018, by = 'LOCATION') %>%
  filter(complete.cases(.)) %>%
  select(country, spending_pct_gdp, life_expectancy) %>%
  arrange(desc(life_expectancy))

head(merged_df)

# Task2
# Generate scatter plot with country names
scatter_plot <- ggplot(merged_df, aes(x = spending_pct_gdp, y = life_expectancy)) +
  geom_point() +
  geom_text_repel(aes(label = country), force = 1, segment.color = 'transparent') +
  labs(x = "Spending (% of GDP)", y = "Life Expectancy") +
  theme_minimal()

# Display the scatter plot
scatter_plot





