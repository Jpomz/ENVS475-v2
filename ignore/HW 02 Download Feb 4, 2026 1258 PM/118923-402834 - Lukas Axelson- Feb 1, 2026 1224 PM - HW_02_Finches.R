# Lukas Axelson ENVS 475 HW 2
# Galapagos Finches Analysis


library(tidyverse)


finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)


# 3.1


# 1.1 Print out the names of all columns
names(finch)

# 1.2 The first two columns are:
# Column 1: year
# Column 2: species

# 1.3 Print the structure of the data
str(finch)

# 1.4 The factor column is "species"
# It has multiple levels corresponding to finch species
levels(finch$species)
nlevels(finch$species)

# 1.5 The mode of the year column is the most frequent year
# 1975 appears more frequently than 2012
table(finch$year)

# 1.6 Display the finch data as a tibble
as_tibble(finch)

# 1.7 Display all values that occur in the year column
unique(finch$year)

# 1.8 Display all combinations of year and species
unique(finch[, c("year", "species")])

# 3.2 Problem 2 – Basic plots

# 2.1 Plot all values in the blength column
plot(finch$blength,
     ylab = "Beak length (mm)",
     xlab = "Individual index")

# 2.2
# Most values cluster between ~8–15 mm, with a few larger values.
# No extreme outliers appear obviously erroneous.

# 2.3 Boxplot of blength by species
plot(blength ~ species,
     data = finch,
     ylab = "Beak length (mm)",
     xlab = "Species")

# 2.4 
# Species differ clearly in median beak length, with scandens
# generally showing longer beaks than fortis.

# 2.5 Plot relationship between blength and bdepth
plot(finch$blength,
     finch$bdepth,
     xlab = "Beak length (mm)",
     ylab = "Beak depth (mm)")

# 2.6 
# There is a positive relationship between beak length and depth,
# suggesting larger beaks tend to be deeper as well.

# 3.3 Problem 3 – Modifying the data


# 3.1 Filter individuals with blength > 15 mm
finch_long <- finch |>
  filter(blength > 15)

finch_long

# 3.2- 13 individuals with blength > 15mm
# This filter returns nrow(finch_long) individuals.
# These individuals occur in the following species and years:
unique(finch_long[, c("species", "year")])

# 3.3 Filter fortis individuals with bdepth > 8 mm
finch_fortis_deep <- finch |>
  filter(species == "fortis",
         bdepth > 8)

nrow(finch_fortis_deep)

# 3.4 Filter individuals with blength exactly 10 or 12 mm
finch_10_12 <- finch |>
  filter(blength == 10 | blength == 12)

nrow(finch_10_12)
finch_10_12

# 3.5 Convert year to a factor with ordered levels
finch_year <- finch |>
  mutate(year = factor(year,
                       levels = c(1975, 2012)))

# Print as tibble
as_tibble(finch_year)

# 3.6 Group by species and year and calculate summary stats
finch_year_means <- finch_year |>
  group_by(species, year) |>
  summarize(
    mean_blength = mean(blength),
    sd_blength   = sd(blength),
    n            = n(),
    se_blength   = sd_blength / sqrt(n)
  ) |>
  ungroup()

finch_year_means
# 3.4 Problem 4 – ggplot

# 4.1 Histogram of bdepth colored by species
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.6,
                 bins = 30) +
  labs(x = "Beak depth (mm)",
       y = "Count")

# 4.2 Faceted histogram by year with viridis colors
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.6,
                 bins = 30) +
  facet_wrap(~year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(x = "Beak depth (mm)",
       y = "Count") +
  theme_bw()

# 4.3 Boxplot of bdepth by year, filled by species
ggplot(finch_year, aes(x = year, y = bdepth, fill = species)) +
  geom_boxplot() +
  labs(x = "Year",
       y = "Beak depth (mm)") +
  scale_fill_viridis_d() +
  theme_bw()

# 4.4 Bar plot of mean beak depth with SE
finch_depth_means <- finch_year |>
  group_by(species, year) |>
  summarize(
    mean_bdepth = mean(bdepth),
    se_bdepth   = sd(bdepth) / sqrt(n())
  ) |>
  ungroup()

ggplot(finch_depth_means,
       aes(x = year,
           y = mean_bdepth,
           fill = species)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_bdepth - se_bdepth,
        ymax = mean_bdepth + se_bdepth),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(y = "Mean beak depth (mm)",
       x = "Year") +
  coord_cartesian(ylim = c(7, 11)) +
  theme_bw()

# 4.5 Scatter plot of beak length vs depth
ggplot(finch_year,
       aes(x = blength,
           y = bdepth,
           color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~year) +
  scale_color_viridis_d() +
  labs(x = "Beak length (mm)",
       y = "Beak depth (mm)") +
  theme_bw()

