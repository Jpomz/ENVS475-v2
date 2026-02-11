#Homework 1
#Katelyn Helcoski

library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
#1.1 ####
names(finch)
#1.2
#year and band
#1.3
str(finch)
#1.4
#Species is a factor with 2 levels which are fortis and scandens
#1.5
mode(finch$year)
#the mode of the year column is numeric
#1.6
finch <- as_tibble(finch)
finch
#1.7
unique(finch$year)
#1.8
distinct(finch, year, species)

#2.1 ####
plot(finch$blength)
#2.2
#there is a good chunk of data that is from roughly 10 to 12, and another from roughly 13 to 14. There could be a few outliers around 16, and 9.
#2.3
plot(finch$species, finch$blength)
#2.4
#there is two data bars showing the median, and quarters for each species beak length, with scandens beak length being longer on average than fortis.
#2.5
plot(finch$blength, finch$bdepth)
#2.6
#there is two major sections that show some correlation that with a higher beak length there is a higher beak depth.

#3.1 ####
finch |>
  filter(blength >= 15)
#3.2
#this command gave me 15 rows, with all species being scandens, and two years of 2012, while the rest are 1975
#3.3
finch |>
  filter(species == "fortis") |>
  filter(bdepth < 8)
#this gives me 35 rows
#3.4
finch |>
  filter(blength == 10 |
           blength == 12)
#3.5
finch |>
  mutate(finch_year = factor(year,
                             levels = c("1975", "2012"))) |>
  tail()
finch_year <- as_tibble(finch)
finch_year
#3.6
finch |>
  group_by(species, finch_year) |>
  summarize(mean_blength = mean(blength), 
            sd_blength = sd(blength),
            n_blength = n()) |>
  mutate(se_blength = sd_blength / sqrt(n_blength)) #something wrong with finch_year

#4.1 ####
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5)
#4.2
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(x = "Beak Depth(mm)")
#4.3
ggplot(data = finch_year,
       aes(x = year,
           y = bdepth,
           fill = species)) +
  geom_boxplot()
#4.4
finch |>
  group_by(species, finch_year) |>
  summarize(mean_blength = mean(blength), 
            sd_blength = sd(blength),
            n_blength = n()) |>
  mutate(se_blength = sd_blength / sqrt(n_blength)) |>
  ggplot(aes(x = finch_year,
             y = mean_blength,
             fill = species,
             ymin = mean_blength - se_blength,
             ymax = mean_blength + se_blength)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4)

#4.5
ggplot(data = finch,
       aes(x = bdepth,
           y = blength)) +
  geom_point(alpha = 0.75, size = 3) +
  facet_wrap(~year) +
  scale_fill_viridis_d() +
  labs(x = Beak lenth (mm),
       y = Beak depth (mm),
       color = species) +
  theme_bw()











