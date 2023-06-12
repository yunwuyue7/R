# clear environment
# it's always a good idea to start fresh with no variables loaded in the R environment
rm(list = ls())


# define the default directory
# it saves you the trouble of having to specify the full path every time you need to load or save a file
# (change the path below to where you have downloaded the covid dataset)
setwd("../")

# load needed libraries
library(readr)
# load and inspect data
# important: we will use read_csv not read.csv
df_covid <- read_csv("owid-covid-data.csv")
str(df_covid)


# Q1: provide solution here (use as many lines of code as you wish)
df_usa <- df_covid[df_covid$location=="United States",]
df_usa_dec <- df_usa[df_usa$date>="2020-12-01" & df_usa$date<"2021-01-01",]
sum(df_usa_dec$new_deaths)


# Q2: provide solution here (use as many lines of code as you wish)
library(dplyr)

df_filtered <- df_covid %>% 
  filter(location %in% c("Mexico", "Canada", "United States"), date == "2020-12-31")

df_total_deaths <- df_filtered %>% 
  group_by(location) %>% 
  summarise(total_deaths = sum(total_deaths))

print(df_total_deaths)


# Q3: provide solution here (use as many lines of code as you wish)
df_end2020 <- df_covid[df_covid$date=="2020-12-31", c("location", "total_cases")]
df_end2020_countries <- df_end2020[!df_end2020$location %in% c("Europe", "World","Asia", "North America","South America","European Union","Africa", "Oceania"),]
df_end2020_ordered <- df_end2020_countries[order(-df_end2020_countries$total_cases),]
head(df_end2020_ordered, 5)


# Q4: provide solution here (use as many lines of code as you wish)
df_dec31 <- df_covid %>% filter(date == "2020-12-31")

df_dec31_deaths <- df_dec31 %>%
  select(location, population, total_deaths) %>%
  filter(!is.na(population), !is.na(total_deaths)) %>%
  mutate(deaths_per_million = total_deaths / (population / 1000000))

df_top5_deaths <- df_dec31_deaths %>%
  arrange(desc(deaths_per_million)) %>%
  head(5)

df_top5_deaths


# Q5: provide solution here (use as many lines of code as you wish)
df_usa <- df_covid[df_covid$location=="United States", c("date", "new_cases")]
df_usa <- na.omit(df_usa)
df_usa[df_usa$new_cases==max(df_usa$new_cases, na.rm=TRUE),]


# Q6: provide solution here (use as many lines of code as you wish)
df_usa <- df_covid %>%
  filter(location == "United States") %>%
  slice_tail(n = 1)
total_fully_vaccinated <- df_usa$people_fully_vaccinated
total_fully_vaccinated


# Q7: provide solution here (use as many lines of code as you wish)
df_usa <- df_covid[df_covid$location=="United States", c("date", "new_vaccinations")]
df_usa_march <- df_usa[df_usa$date>="2021-03-01" & df_usa$date<"2021-04-01",]
mean(df_usa_march$new_vaccinations)


# Q8: provide solution here (use as many lines of code as you wish)
library(ggplot2)
df_world <- filter(df_covid, location == "World")
df_cases <- select(df_world, date, new_cases)
par(mar=c(3,3,2,2)) 
plot(df_cases$date, df_cases$new_cases, type = "l", xlab = "Date", ylab = "Daily New Cases", main = "Daily Covid Cases in the World")


# Q9: provide solution here (use as many lines of code as you wish)
df_pop_filter <- df_covid %>%
  filter(population > 50000000)

df_date_filter <- df_pop_filter %>%
  filter(date == "2021-03-01")

df_sorted <- df_date_filter %>%
  arrange(total_cases)

df_result <- df_sorted[1, c("location", "total_cases")]
print(df_result)


# Q10: provide solution here (use as many lines of code as you wish)
df_filtered <- df_covid %>% 
  filter(grepl("land$", location))

df_result <- df_filtered %>% 
  select(location, date, total_cases)

df_result <- df_result %>% 
  filter(date == "2020-12-31")

df_result


