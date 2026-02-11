# homework 1 script

library(tidyverse)

finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)

# 1.1 
names(finch)

# 1.2
# "year" and "band"

# 1.3
structure(finch)

# 1.4 
str(finch)
# species is a factor
# 2 levels "fortis" and "scandens"

# 1.5
mode(finch$year)
# numeric

# 1.6
as_tibble(finch)

# 1.7
distinct(finch, year)
# 2012 and 1975

# 1.8
distinct(finch, year, species)
# 2012, fortis; 2012, scandens; 1975, fortis; 1975, scandens

# 2.1
plot(finch$blength)

# 2.2
# There are almost clear groups of finches with similar beak sizes, 4 to be specific. 
#The only points that look concerning are the very small beaks, as beaks are a birds main tool and a small beak may limit the bird's ability to eat.

# 2.3
plot(finch$species, finch$blength)

# 2.4
# I see two boxes each with two outlier points. 
# Fortis having smaller beaks than scandens. 

# 2.5 
plot(finch$bdepth, finch$blength)

# 2.6
# I see two groupings of data, one with a smaller beak length and then one with a longer beak length. 
# Both groups have a positive trend line.

# 3.1
finch |>
  filter(blength >= 15)

# 3.2
# There are 15 rows/ individuals.
# 2012 scandens and 1975 scandens

# 3.3 
finch |>
  filter(species == "fortis", bdepth < 8)
# 35 rows

# 3.4 
finch |>
  filter(blength == 10.00 | blength == 12.00)

# 3.5
finch_year <- finch |>
  mutate(finch_year = factor(year,
                             levels = c("1975", "2012")))
tibble(finch_year)

# 3.6
finch_year_means <- finch_year |>
  group_by(species, year) |>
  summarize(mean_length = mean(blength),
            sd_length = sd(blength),
            n_length = n()) |>
  mutate(se_length = sd_length / sqrt(n_length))

finch_year_means

# 4.1
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.75)

# 4.2
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  labs(x = "Beak Depth (mm)")+
  geom_histogram(position = "identity",
                 alpha = 0.75) +
  scale_fill_viridis_d(option = "magma", begin = 0.8, end = 0.5) +
  theme_bw()

# 4.3
ggplot(data = finch_year,
       aes(x = factor(year),
           y = bdepth,
           fill = species)) +
  geom_boxplot() +
  scale_color_viridis_d(option = "mako", begin = 0.2,
                        end = 0.8) +
  theme_bw() +
  labs(x = "Year", y = "Beak Depth", fill = "Species")

# 4.4
finch_year |>
  group_by(year, species) |>
  summarize(mean_depth = mean(bdepth),
            sd_depth = sd(bdepth),
            n_depth = n()) |>
  mutate(se_depth = sd_depth / sqrt(n_depth)) |>
  ungroup() |>
  ggplot(aes(x = year,
             y = mean_depth,
             fill = species,
             ymin = mean_depth - se_depth,
             ymax = mean_depth + se_depth)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.6),
                width = 0.2) +
  scale_fill_viridis_d(option = "mako", 
                       begin = 0.2, 
                       end = 0.7, 
                       alpha = 0.6) +
  theme_bw() +
  labs(y = "Mean beak depth (mm)") +
  coord_cartesian(ylim = c(6, 11))

# 4.5
finch_year |>
  ggplot(aes(x = blength,
             y = bdepth,
             color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ year) +
  scale_color_viridis_d(option = "plasma",
                        begin = 0.2,
                        end = 0.8) +
  theme_minimal() +
  labs(x = "Beak length (mm)",
    y = "Beak depth (mm)",
    color = "Species")









