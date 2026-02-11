# Ivy Zipp
# Homework 2
library(tidyverse)

finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)

# problem 1 ####
# 1.1
names(finch)

# 1.2
# "year" and "band"

# 1.3
str(finch)

# 1.4
# column 3 / "species"
# 2 levels
# "fortis" and "scandens"

# 1.5
# integer

# 1.6
as_tibble(finch)

# 1.7
select(finch, "year")

# 1.8
distinct(finch, year, species)
  
# problem 2 ####
# 2.1
plot(finch$blength)

# 2.2
# The points are in four groups, with two groups of shorter beaks and two groups of longer beaks. These two groups likely correlate to the two species of finch studied. It seems that different numbers of each species were studied in 1975 and 2012 because one of the shorter beak groups is larger than the other. I don't see any clearly concerning data points, but if the data was grouped by species instead of year it might be easier to tell. 

# 2.3
plot(finch$species, finch$blength)

# 2.4
# The fortis species have shorter beaks, with an average around 10.5 mm. The scandens species have longer beaks, with an average just bellow 14 mm. The standard deviations of both species' data appear similar, as do their ranges.

# 2.5
plot(finch$blength, finch$bdepth)

# 2.6
# The two species are fairly distinct, with the fortis points on the left and scandens on the right. The fortis have a wider range of beak depths and a clearer positive correlation of beak length to depth. The scandens also appear to trend toward deeper beaks as length increases, but there is higher variance. 

# problem 3 ####
# 3.1 
filter(finch, blength >= 15)

# 3.2
# 15

# 3.3
filter(finch, species == "fortis",
       bdepth < 8)
# 35

# 3.4
filter(finch, blength == 10 |
         blength == 12)
# 3.5
finch_year <- finch|>
  mutate(year = factor(year,
                       levels = c("1975", "2012")))

as_tibble(finch_year)

# 3.6 
finch_year_means <- finch_year|>
  group_by(species, year)|>
  summarize(l_avg = mean(blength),
            l_sd = sd(blength),
            l_n = n())|>
  mutate(l_se = l_sd / sqrt(l_n))|>
  ungroup()

finch_year_means  

# problem 4 ####
# 4.1
ggplot(finch,
       aes(x = bdepth))+
  geom_histogram(position = "identity",
                 alpha = 2)

# 4.2
ggplot(finch,
       aes(x = bdepth,
           fill = species))+
  geom_histogram(position = "identity",
                 alpha = 2)+
  facet_wrap(~year)+
  labs(x = "Beak Depth (mm)")+
  scale_fill_viridis_d(option = "mako",
                       begin = 0.3,
                       end = 0.9)+
  theme_bw()

# 4.3
ggplot(finch_year,
       aes(x= year,
           y = bdepth,
           fill = species))+
  geom_boxplot()+
  scale_fill_viridis_d(option = "mako",
                       begin = 0.3,
                       end = 0.9)

# 4.4
ggplot(finch_year_means,
       aes( x = year,
            fill = species,
            y = l_avg,
            ymin = l_avg - l_se,
            ymax = l_avg + l_se))+
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4)+
  scale_fill_viridis_d(option = "mako",
                       begin = 0.3,
                       end = 0.9)+
  labs(y = "Mean Beak Length (mm)",
       x = "Year")+
  coord_cartesian(ylim = c(8, 14.5))

# 4.5
ggplot(finch_year, 
       aes(x = blength,
           y = bdepth,
           color = species))+
  geom_point()+
  facet_wrap(~year)+
  scale_color_viridis_d(option = "mako",
                        begin = 0.3,
                        end = 0.8)+
  labs(x = "Beak Length (mm)",
       y = "Beak Depth (mm)")

