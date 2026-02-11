#homework script 
library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
finch

#problem 1.1 ####
names(finch)

# 1.2
#year, band

# 1.3
str(finch)

# 1.4 
# the species column is a factor with 2 levels (fortis and scandens) 

# 1.5
# the mode of the year column is "2012" 

# 1.6
as_tibble(finch)

# 1.7
unique(finch$year)

# 1.8
distinct(finch, year, species)

#problem 2.1 ####
plot(finch$blength)

# 2.2
# the data looks bunched between 9mm and 12mm for index values 0-100 and 250-550.It is also bunched between 12mm and 16mm for index values 100-250 and 220-700.  
# 2.3
plot(finch$species, finch$blength)

# 2.4
# I can see that the Fortis species has a lower average beak length (10-11mm) than Scandens(13-14mm) 

# 2.5
plot(finch$blength, finch$bdepth)

# 2.6
# I can see that there are two groups of data that are bunched together. The depth appears to increase as the length increases. 

# 3.1 ####
finch |>
  filter(blength >= 15)

# 3.2
# there are 15 rows from this code. They are all scandens and from the years 2012 and 1975

# 3.3 
finch |>
  filter(species == "fortis",
         bdepth < 8)
# this returned 35 rows 

# 3.4
finch |>
  filter(blength == 10 |
           blength == 12)
# 3.5
finch_year <- finch |> 
  mutate(year = factor(year,
                             levels = c(1975, 2012)))
as_tibble(finch_year)

# 3.6
finch_year_means <- finch_year |>
  group_by(species, year) |>
  summarize(mean_bdepth = mean(bdepth),
            sd_bdepth = sd(bdepth),
            n_bdepth = n()) |>
  mutate(se_bdepth = sd_bdepth / sqrt(n_bdepth)) |>
  ungroup()

# 4.1 ####
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.75)

# 4.2
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(position = "identity",
                 alpha = 0.75) +
  facet_wrap(~year) +
  scale_fill_viridis_d(option = "magma",
                       end = 0.6) +
theme_bw() +
  labs(x = "Beak depth (mm)")

# 4.3
ggplot(data = finch_year,
       aes(x = year,
           y = bdepth,
           fill = species)) +
  geom_boxplot()

# 4.4
ggplot(data = finch_year_means,
       aes(x = species,
           y = mean_bdepth,
           fill = year,
           ymin = mean_bdepth - se_bdepth,
           ymax = mean_bdepth + mean_bdepth)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4) +
scale_fill_viridis_d(option = "mako",
                     end = 0.6) +
  theme_bw() +
  labs(y = "Mean beak depth (mm)") 

# 4.5
ggplot(data = finch_year,
       aes(x = blength,
       y = bdepth,
       color = species)) +
  geom_point(alpha = 0.7, size = 3) +
  facet_wrap(~year) +
  scale_color_viridis_d(option = "plasma",
                        end = 0.8) +
  labs(x = "Beak length (mm)",
       y = "Beak depth (mm)",
       color = "Species") +
  theme_bw()







