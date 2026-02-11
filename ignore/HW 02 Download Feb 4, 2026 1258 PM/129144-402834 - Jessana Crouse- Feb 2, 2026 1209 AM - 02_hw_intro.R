# hw 02 Jess Crouse
library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
# problem 1.1
names(finch)
# problem 1.2
# year, band
# problem 1.3
str(finch)
# problem 1.4
# species is the factor. there are two levels- "fortis" and "scandens"
finch |>
  summarize(mode_year = mode(year))
# problem 1.5
# the mode of the year column is numeric
# problem 1.6
finch <- as_tibble(finch)
finch
# problem 1.7
unique(finch$year)
# problem 1.8
distinct(finch, year, species)

# problem 2.1
plot(finch$blength)
# problem 2.2
# there are a lot of clusters of data points at different lengths, none are too different from the others though
# problem 2.3
plot(finch$species, finch$blength)
# problem 2.4
# the scandens species typically has a longer beak than the fortis species
# problem 2.5
plot(finch$blength, finch$bdepth)
# problem 2.6
# typically, the longer a beak is, the more depth it has as well up to 12 mm, and then the pattern starts over and repeats with longer beaks

# problem 3.1
finch |>
  filter(blength >= 15)
# problem 3.2
# the code returned 15 rows. scandens is the only species in this filter, with both 2012 and 1975 as years
# problem 3.3
finch |>
  filter(species == "fortis", bdepth < 8)
# this code returns 35 rows
# problem 3.4
finch |>
  filter(blength == 10 | blength == 12)
# problem 3.5
finch |>
  mutate(finch_year = factor(year,
                             levels = c("1975", "2012")))
finch_year <- finch |>
  mutate(finch_year = factor(year,
                             levels = c("1975", "2012")))
# problem 3.6
finch |>
  group_by(species, year) |>
  summarize(mean_blength = mean(blength),
            sd_blength = sd(blength),
            n_blength = n()) |>
  mutate(se_blength = sd_blength / sqrt(n_blength)) 
finch_year_means <- finch |>
  group_by(species, year) |>
  summarize(mean_blength = mean(blength),
            sd_blength = sd(blength),
            n_blength = n()) |>
  mutate(se_blength = sd_blength / sqrt(n_blength))
finch_year_means
ungroup(finch_year_means)
# problem 4.1
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5)
# problem 4.2
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_viridis_d() +
  theme_bw()
# problem 4.3
ggplot(data = finch_year,
       aes(x = bdepth,
           fill = species)) +
  geom_boxplot()
# problem 4.4
finch_year_means |>
  group_by(species, year) |>
  ggplot(aes(x = n_blength,
             y = mean_blength,
             fill = species,
             ymin = mean_blength - se_blength,
             ymax = mean_blength + se_blength)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4) +
  facet_wrap(~year) +
  scale_fill_viridis_d() +
  theme_bw() +
  labs(y = "mean beak length (mm)")
# problem 4.5
finch_year |>
  group_by(species, year) |>
  ggplot(aes(x = blength,
             y = bdepth,
             color = species)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_viridis_d(option = "magma",
                        end = 0.6) +
  theme_bw() +
  labs(y = "beak depth (mm)",
       x = "beak length (mm)")









