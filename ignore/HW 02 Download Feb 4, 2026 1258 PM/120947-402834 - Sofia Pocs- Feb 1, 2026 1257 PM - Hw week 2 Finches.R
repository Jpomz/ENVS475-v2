# Homework week 2 
library(tidyverse)
finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
# problem 3.1  1.1 
names(finch)
# 1.2
# "year" "band" 
# 1.3 
str(finch)
# 1.4
# 651 obs, 5 variables: year, band, species is a factor, blength, bdepth
levels(finch$species)
# "fortis", "scandens" 
# 1.5 
table(finch$year)
# 1975 = 403, 2012 = 248 
# 1.6 
library(tibble, 
        finch)
# 1.7 
unique(finch$year)
# 1.8 
unique(finch, 
       c("year","species"))
# unsure why that did not work. 



# problem 3.2  2.1
plot(finch$blength)
# 2.2 
# the data is very mixed but in clumps 
# there are 4 distinct clumps of beak length 
# data all across the board 
# only "concerning" would be the points not really in a "clump" 
# points near (12,100)
# 2.3 
plot(blength ~ species, 
     data = finch)
# 2.4 
# fortis is less than scandens 
# 2.5 
plot(finch$blength, finch$bdepth)
# 2.6 
# depth to length all gradually are upwards but has lots of outliers 


# problem 3.3  3.1 
# I have no idea how to do this 
filter(blength>15)
filter(blength >= 15)
# 3.2 
# i do not know how to get this data 
# 3.3 
  filter(species == "fortis", bdepth >= 8)
# 3.4 
filter(blength == 10 | blength == 12)
# don't know 
# 3.5 and 3.6 no idea how to do 


# problem 3.4  4.1
# feel good about this section because we just did it
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(
    x = "Beak depth (mm)",
    y = "Count"
  ) +
  theme_minimal()
# 4.2 
ggplot(finch, aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  facet_wrap(~ year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Beak depth (mm)",
    y = "Count"
  ) +
  theme_bw()
# 4.3 
ggplot(finch_year, aes(x = year, y = bdepth, fill = species)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Year",
    y = "Beak depth (mm)"
  ) +
  theme_bw()
# not sure why that does not work, maybe because in part 3? 
# 4.4 
ggplot(finch_year_means,
       aes(x = year, y = mean_blength, fill = species)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(position = position_dodge(0.9 +
                width = 0.9)aes( ymin = mean_blength - se_blength,
                ymax = mean_blength + se_blength) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Year",
    y = "Mean beak length (mm)"
  ) +
  coord_cartesian(ylim = c(8, 14)) +
  theme_bw()
# I DON"T KNOW WHAT IS HAPPENING 
# 4.5 
ggplot(finch,
       aes(x = blength, y = bdepth, color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ year) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Beak length (mm)",
    y = "Beak depth (mm)"
  ) +
  theme_bw()
# where is Finch year, why can't it find it
# this is making me angry. 
# i changed finch_year to just finch, does that work? 

# 4.3 again 
ggplot(finch, aes(x = year, y = bdepth, fill = species)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(
    x = "Year",
    y = "Beak depth (mm)"
  ) +
  theme_bw()

# So i am unsure where "finch year" is, but just finch works? 


