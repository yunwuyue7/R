setwd("../")

library(tidyverse)
library(ggrepel)
library(ggplot2)
library(cluster)

# Read the data
df1 <- read_csv("healthcare_spending.csv")
df2 <- read_csv("life_expectancy.csv")
df3 <- read_csv("country_codes.csv")

# Data preprocessing
# At the beginning, I encountered an error in this place. The reason is that the functions filter, 
# mutate, and select are defined in different packages, causing errors, so you need to add the prefix of dplyr::
df1_reduced <- df1 %>%
  dplyr::filter(TIME == 2018, MEASURE=="PC_GDP", SUBJECT=="TOT") %>%
  dplyr::mutate(spending_pct_gdp = Value) %>%
  dplyr::select(LOCATION, spending_pct_gdp)

df2_reduced <- df2 %>%
  filter(TIME == 2018, INDICATOR =="LIFEEXP", SUBJECT=="TOT") %>%
  select(LOCATION, life_expectancy = Value)

df_final <- df1_reduced %>%
  inner_join(df2_reduced) %>%
  left_join(df3, by=c("LOCATION" = "CODE")) %>%
  select(Country, spending_pct_gdp, life_expectancy) %>%
  arrange(desc(life_expectancy)) %>%
  rename(country = Country)

# Perform k-means clustering
data <- df_final[, c("spending_pct_gdp", "life_expectancy")]
set.seed(123)  # For reproducibility
k_values <- 1:10
wss <- sapply(k_values, function(k) {
  kmeans(data, centers = k, nstart = 25)$tot.withinss
})

# Elbow plot
elbow_plot <- data.frame(k = k_values, wss = wss)
ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters (k)", y = "Total Within Sum of Squares") +
  ggtitle("Elbow Plot") +
  theme_bw()

# Determine the optimal number of clusters using the elbow method
# Identify the "knee" of the plot where the reduction in WSS begins to level off
# Choose the corresponding k value

# Set the chosen k value based on the elbow analysis
chosen_k <- 3

# Perform k-means clustering with the chosen k value
clusters <- kmeans(data, centers = chosen_k, nstart = 25)

# Add cluster information to the dataframe
df_final$cluster <- as.factor(clusters$cluster)

# Plot with cluster information
df_final %>%
  ggplot(aes(x = spending_pct_gdp, y = life_expectancy, label = country, color = cluster)) +
  geom_point(alpha = 0.2) +
  geom_text_repel(size = 2.5, repel = TRUE) +
  labs(y = "Life Expectancy (years)", x = "Healthcare Spending (% GDP)", color = "Cluster") +
  theme_bw()
