# homework script

library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)

# problem 1.1 
names(finch)

# problem 1.2
# The answer is species then year

# problem 1.3
str(finch)

# problem 1.4
# The factor column is "species"
# Number of levels:2
nlevels(finch$species)


# problem 1.5
mode(finch$year)
# answer is numeric

# problem 1.6
as_tibble(finch)

# problem 1.7
unique(finch$year)

# problem 1.8
distinct(finch,year,species)

#problem 2.1
plot(finch$blength)

#problem 2.2
#The data does not appear to follow any trend but there is no obvious outlines. 

# problem 2.3
plot(blength ~ species, data = finch)

# problem 2.4
# The species Scandens have a higher median beak length.
# there is lots of variation in the data and overlap which indicates there is-
# - similarities in the data

# problem 2.5
plot(finch$blength, finch$bdepth,
     xlab = "Beak length (mm)",
     ylab = "Beak depth (mm)")

#problem 2.6
# there is no obvious relationship when comparing blength and bdepth. 
# there might be a slight increase in beak depth as beak length increases

#problem 3.1
finch |>
  filter(blength>15)

#problem 3.2
# there are 13 rows
# there is the species scandens and the years 1975 and 2012

# problem 3.3
finch |>
  filter(species=="fortis", bdepth<8)
# there are 35 rows

# problem 3.4
finch |>
  filter(blength==10|
         blength==12)

# problem 3.5
finch_year<-finch |>
  mutate (year = factor(year, levels = c(1975, 2012)))
as_tibble(finch_year)

#problem 3.6
finch_year_means <- finch_year|>
  group_by(species, year) |> 
  summarise(
    mean_blength = mean(blength),
    sd_blength = sd(blength),
    n = n(),
    se_blength = sd_blength / sqrt(n)
  ) |>
  ungroup()

# problem 4.1
ggplot(data=finch,
       aes(x=blength,
           fill=species))+ 
  geom_histogram(position="identity",
                 alpha=.85)

#problem 4.2
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.85,) +
  facet_wrap(~year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8,option="magma") +
  labs(x = "Beak depth (mm)") +
  theme_bw()

#problem 4.3
ggplot(finch_year, aes(x = year, y = bdepth, fill = species)) +
  geom_boxplot() +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option="magma") +
  labs(x = "Year", y = "Beak depth (mm)") +
  theme_bw()

#problem 4.4
ggplot(finch_year_means,
       aes(x = species, y = mean_blength, fill = year)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_blength - se_blength,
        ymax = mean_blength + se_blength),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8,option="magma") +
  labs(y = "Mean beak length (mm)", x = "Species") +
  coord_cartesian(ylim = c(min(finch_year_means$mean_blength) - 1,
                           max(finch_year_means$mean_blength) + 1)) +
  theme_minimal()

#problem 4.5
ggplot(finch_year, aes(x = blength, y = bdepth, color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~year) +
  scale_color_viridis_d(begin = 0.2, end = 0.8,option="magma") +
  labs(
    x = "Beak length (mm)",
    y = "Beak depth (mm)",
    color = "Species"
  ) +
  theme_bw()




