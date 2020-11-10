library(tidyverse)
library(scales)
library(ggridges)
library(hrbrthemes)

# Get the Data

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# Brewers by Year

brewer_size_clean <- brewer_size %>% 
  filter(brewer_size != "Total")

brewer_size_clean$brewer_size <-
  factor(
    brewer_size_clean$brewer_size,
    levels = c(
      "6,000,001 Barrels and Over",
      "2,000,000 to 6,000,000 Barrels",
      "1,000,001 to 6,000,000 Barrels",
      "1,000,000 to 6,000,000 Barrels",
      "1,000,001 to 1,999,999 Barrels",
      "500,001 to 1,000,000 Barrels",
      "100,001 to 500,000 Barrels",
      "60,001 to 100,000 Barrels",
      "30,001 to 60,000 Barrels",
      "15,001 to 30,000 Barrels",
      "7,501 to 15,000 Barrels",
      "1,001 to 7,500 Barrels",
      "1 to 1,000 Barrels",
      "Under 1 Barrel",
      "Zero Barrels"
    ),
    labels = c(
      "6,000,001+",
      "1,000,001 to 6,000,000+",
      "1,000,001 to 6,000,000+",
      "1,000,001 to 6,000,000+",
      "1,000,001 to 6,000,000+",
      "500,001 to 1,000,000",
      "100,001 to 500,000",
      "60,001 to 100,000",
      "30,001 to 60,000",
      "15,001 to 30,000",
      "7,501 to 15,000",
      "1,001 to 7,500",
      "1 to 1,000",
      "Under 1 Barrel",
      "Zero Barrels"
    )
  )

brewer_size_clean %>% 
  filter(!brewer_size %in% c("Zero Barrels", "Under 1 Barrel", "1 to 1,000")) %>% 
  mutate(microbrew = if_else(brewer_size == "7,501 to 15,000 Barrels", TRUE, FALSE)) %>% 
  ggplot(aes(year, n_of_brewers, color = brewer_size)) +
  geom_line() +
  scale_color_manual(values = c("gray80",
                                "gray80",
                                "gray80",
                                "gray80",
                                "gray80",
                                "gray80",
                                "gray80",
                                "gray80",
                                "red")) +
  scale_y_continuous(labels = comma) +
  labs(title = "The Meteoric Rise of the Microbrewer",
       x = "Year",
       y = "Number of Brewers",
       color = "Brewer Size (barrels)") +
  theme_minimal() +
  NULL
