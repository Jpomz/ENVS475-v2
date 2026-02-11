# Homework 2

library(tidyverse)

finch <- read.csv("data/galapago_finches.csv",stringsAsFactors = TRUE)

head(finch)

# 1.1
names(finch)

# 1.2
# "year, "band

# 1.3
str(finch)

#1.4
# Species with two levels, "fortis" and "scandens"

#1.5
# Integer

#1.6
as_tibble(finch)

#1.7
unique(finch$year)

#1.8
distinct(finch, species, year)

# Problem 2

#2.1
plot(finch$blength)

#2.2
# The plot shows four different clusters of finch blengths across index. One plot between index 0-100 at 10 mm, one at index ~120 to ~250 at 14 mm, a larger cluster from index 250 to 550 at 10mm and one at index 600 at 14mm. 

#2.3
plot(finch$species, finch$blength)

#2.4
# The plot above shows species fortis resting at ~11 blength with error bars, and the scandens species resting at a blength of ~14.

#2.5
plot(finch$blength, finch$bdepth)

#2.6
# In the plot above there is are two clusters of data which both displays linear relationship. The first cluster, the b depth drops off when finch b length = 12, then increases again after b length of 12.

# Problem 3

#3.1
finch|>
  filter(blength >= 15)

# 3.2
# There are 15 rows, the only species in this set is "scandens" and the years are 2012, and 1975.

#3.3
finch|>
  filter(bdepth< 8,
         species == "fortis")
#3.4
finch|>
  filter(blength== 10 |
           blength== 12)
#3.5
finch_year <- finch|>
  mutate(year= factor(year,
                            levels= c("1975", "2012")))
as_tibble( finch_year)
str(finch_year)

#3.6
finch_year_means <- finch_year|>
  group_by(species,year)|>
  summarize(mean_blength=mean(blength),
            sd_blength=sd(blength),
            n_blength=n())|>
  mutate(se_blength= sd_blength/ sqrt(n_blength))|>
  ungroup()

as_tibble(finch_year_means)  

# Problem 4

# 4.1
ggplot(data= finch, mapping= aes( x= bdepth,
                                  fill = species))+
  geom_histogram(position = "identity",
                 alpha= 0.5)
# 4.2
ggplot(data= finch, mapping= aes( x= bdepth,
                                  fill = species))+
  geom_histogram(position = "identity",
                 alpha= 0.5)+
  facet_wrap(~year)+
  labs(x= "Beak depth (mm)")+
  scale_fill_viridis_d()+
  theme_bw()

# 4.3
ggplot(data= finch_year, aes(x = year,
                             y= bdepth,
                             fill = species))+
  geom_boxplot()

#4.4
ggplot(data= finch_year_means,
       aes(x=year,
          fill=species,
          y= mean_blength,
          ymin= mean_blength - se_blength,
          ymax= mean_blength + se_blength))+
  geom_col(position = "dodge")+
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4) +
  coord_cartesian(ylim = c(9, 15))

#4.5

ggplot(data= finch_year, aes( x= blength,
                         y= bdepth,
                         color= species))+
  geom_point() +
  facet_wrap(~year)+
  labs(x= "Beak Length (mm)",
       y = "Beak Depth (mm)")+
  theme_bw()+
  scale_fill_viridis_d()



































































