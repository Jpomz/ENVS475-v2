#Homework Script 02
library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
finch
# Problem Section 1 ####
# 1.1
names(finch)
# 1.2 (first 2 columns year and band)
# 1.3
str(finch)
# 1.4 species column is a factor and the levels are year,band,species,blength,bdepth
mode(year)
#v1.5 year is a "function
# 1.6
tibble(finch)
# 1.7
unique(finch$year)
# 1.8
unique(finch[, c("year","species")])
# Problem Section 2 ####
# 2.1
plot(finch$blength,
     ylab = "Beak length",
     xlab = "Individual",
     main = "Beak Length Values")
# 2.2
# the main part of the data that stands out to me is the huge jumps between 100-250 and 550-600
#2.3
plot(blength ~ species,
     data = finch,
     ylab = "Beak length",
     xlab = "Species",
     main = "Beak Length by Species")
# 2.4
# things i notice in the plot are that there is little overlap and the diffrence between the two are vast
#2.5
plot(finch$bdepth, finch$blength,
     xlab = "Beak depth",
     ylab = "beak length",
     main = "Relationship Between Beak Length and Depth")
#2.6
# the data goes up in a positive sense but also is very vastly spread out
# Problem Section 3 ####
library(dplyr)
#3.1
finch_long <- finch %>%
  filter(blength >= 15)
#3.2
# there are 15 individuals and years of 2012 and 1975
#3.3
finch_fortis_shallow <- finch %>%
  filter(species == "fortis", bdepth < 8)
#3.3 35 rows for this one
#3.4
finch_fortis_shallow <- finch %>%
  filter(blength == 10 | blength == 12)
#3.5
finch_year <- finch %>%
  mutate(year = factor(year, levels = c(1975, 2012)))
tibble(finch_year)
#3.6
finch_year_means <- finch_year %>%
  group_by(species, year) %>%
  summarise(
    mean_blength = mean(blength),
    sd_blength = sd(blength),
    n = n(),
    sd_blength + sd_blength / sqrt(n)) %>%
  ungroup()

finch_year_means
# Problem Sector 4 ####
library(ggplot2)
#4.1
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  labs(
    x = "Beak depth",
    y = "Count"
  )
#4.2
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Beak depth",
    y = "Count"
  ) +
  theme_bw()
#4.3
ggplot(finch_year, aes(x = year, y = bdepth, fill = species)) +
  geom_boxplot() +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Years",
    y = "Beak depth"
  ) +
  theme_bw()
#4.4 ive spent all weekend on this and cant figure it out what is wrong to save my life
ggplot(finch_year_means,
         aes(x = year, y = mean_blength, fill = species)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(
      ymin = mean_blength - se_blength,
      ymax = mean_blength + se_blength
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Year",
    y = "Mean beak length"
  ) +
  coord_cartesian(ylim = c(11, 14)) +
  theme_bw()
#4.5
ggplot(finch_year,
       aes(x = bdepth, y = blength, color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ year) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Beak depth",
    y = "Beak length",
    color = "Species"
  ) +
  theme_bw()

