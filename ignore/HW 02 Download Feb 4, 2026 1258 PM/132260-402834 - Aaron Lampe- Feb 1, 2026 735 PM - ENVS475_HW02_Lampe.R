# HW 02, Lampe

library(tidyverse)

finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
finch

#3.1 ####

#Problem 1.1
names(finch)

#1.2
#year , band 

#1.3
str(finch)

#1.4
#The species row is a factor, 
unique(finch$species)
# the levels are fortis and scandens 

#1.5
# the mode of the year column is integers 

#1.6
as_tibble(finch) 

#1.7
finch$year

#1.8
distinct(finch, year, species)

#3.2 ####

#2.1 
plot(finch$blength)

#2.2 
# the data has two distinct leves of data, the first and thrid are on one level and the second and fourth are on another. I don't see any concerning data 

#2.3
plot(blength ~ species, data = finch)

#2.4
# I see to plots with means and averages as well as a few outliers

#2.5
plot(blength ~ bdepth, data = finch)

#2.6
#There are relations between short and long beak length, there is very few in between beak length, the shorter the beak the smaller the depth but the depth is limited by length.

# 3.3 ####

#3.1
finch |>
  filter(blength >= 15)
#3.2
# There are 15 individual rows, only scandens are there and 13 are from 1975 and 2 are from 2012

#3.3
finch |> 
    filter(bdepth <8,
         species == "fortis")
# this returns 35 rows

#3.4
finch |>
  filter(blength == 10 |
           blength == 12)

#3.5
finch2 <- finch |>
  mutate(finch_year = factor(year,
                             c("1975", "2012")))
as_tibble(finch2)
levels(finch2$finch_year)

#3.6
finch2 |>
  group_by(finch_year) |> # only the categories here
  summarize(mean_blength = mean(blength),
            sd_mass = sd(blength),
            n_mass = n()) |>
  mutate(se_mass = sd_mass / sqrt(n_mass))|>
  ungroup()


#3.4 Problem 4, ####

#4.1
finch
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(alpha = .5,
                 position = "identity")

#4.2
finch
ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
  geom_histogram(alpha = .5,
                 position = "identity") +
  facet_wrap(~year)+
  labs(x = "beak depth (mm)")+
  geom_col()
