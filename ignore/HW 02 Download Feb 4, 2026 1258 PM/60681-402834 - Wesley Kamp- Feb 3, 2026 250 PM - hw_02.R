# homework script
library(tidyverse)

finch <-  read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
finch

# problem 1.1
names(finch)
# problem 1.2 "year "band"
# problem 1.3 [1] "year"    "band"    "species" "blength" "bdepth" 

str(finch)
sapply(finch, is.factor)
# 1.4 column that is a factor is species, shows TRUE and is not a number, but a stated "thing". It has 2 levels: "fortis", "scandens"

mode(finch$year)
# 1.5 the mode of the year column is numeric

head(finch)
as_tibble(finch)
finch <- as_tibble(finch)
finch
names(finch)
unique(finch$year)
finch$year
unique(finch$species)

distinct(finch, year, species)

plot(finch$blength)
# 2.2 I see that beaks in the 100-250 and 575-675 range are trending longer, the concerning point is how low the shortest beak lengths are between finches 1-100 and 250-575.

plot(finch$species, finch$blength)
# 2.4 The scandens finch has a longer beak length than the fortis finch. The median for the fortis finch is nearly 10.5 mm and 50% of the fortis finches beaks are above 10 mm, with a few outliers above 13 mm. Scanden finches have a median beak length of about 14, and 50% of their beak lengths are 13.5~14 mm. There are two outliers, one below 12 mm and one 16 mm.

plot(blength~bdepth, data = finch)
# 2.6 When beak length is 10-11 mm there is a consistent gradual increase in beak depth from 8-10 mm. There are two distinct groups of finches with beak lengths that range from about 10-11 mm and 12-14.5 mm. There is also a positive correlation between the two, as beak depth increases so does beak length.


finch
finch |>
  filter(blength >= 15)
# 3.2 This code shows 15 rows of beak lengths greater/equal to 15. All the species are scandens and span from 1975 to 2012

finch |>
  filter(species == "fortis",
         bdepth < 8)
# 3.3 This filter shows me 35 rows


finch |>
  filter(blength == "10" |
           blength == "12")
# 3.4 23 rows!


finch2 <- finch |>
  mutate(finch_year = factor(year,
                             levels = c("1975", "2012")))
as_tibble(finch2)
# 3.5 printed into console

finch
finch2




# 3.6
finch_year_means <- finch2 |>
  group_by(species, finch_year) |>
  summarize(
    # new_name = function(column)
     mean_mass = mean(blength),
            sd_mass = sd(blength),
            n_mass = n()) |>
  mutate(se_mass = sd_mass / sqrt(n_mass)) |>
  ungroup()

finch_year_means




# 4.1

finch

ggplot(data = finch,
       aes(x = bdepth,
           fill = species)) +
         geom_histogram(position = "identity", 
                        alpha = 0.75)


# 4.2

ggplot(data = finch2,
       aes(x = bdepth, fill = species)) +
  geom_histogram(position = "identity", 
                 alpha = .75) +
  facet_wrap(~year) + 
  scale_fill_viridis_d(option = "turbo", 
                       begin = 0.75) +
  theme_bw() +
  scale_x_continuous(name = "Beak Depth (mm)")


# 4.3
ggplot(data = finch2,
       aes(x = finch_year, 
           fill = species,
           y = bdepth)) +
  geom_boxplot()


# 4.4
finch_year_means |>
  ggplot(aes(x = finch_year,
             y = mean_mass,
             fill = species,
             ymin = mean_mass - se_mass,
             ymax = mean_mass + se_mass)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.4)

# 4.5 

finch2 |> 
  ggplot(aes(x = blength,
             y = bdepth,
             color = species)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~finch_year) +
  scale_color_viridis_d(option = "turbo",
                        end = 0.78) +
  theme_bw() +
  labs(x = "Beak Length (mm)",
       y = "Beak Depth (mm)")


# from notes used as reference
#      aes(x = Mass.final, # bdpeth blength
 #          y = SVL.final,
  #         color = Pred)) + # species
  #geom_point(alpha = 0.5) +
  #facet_wrap(~Res) + # year
  #scale_color_viridis_d(option = "mako",
   #                     end = 0.6) +
  #theme_bw()
