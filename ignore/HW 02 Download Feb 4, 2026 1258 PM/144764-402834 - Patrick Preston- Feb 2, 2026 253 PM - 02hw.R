# homework 2 script

library(tidyverse)

finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
# 1 ####
# 1.1
names(finch)
# 1.2 names of first two columns: year, band
# 1.3
str(finch)
# 1.4 $ species is a factor, 2 levels: fortis, scandens
mode(finch$year)
# 1.5 year has a numeric mode
# 1.6
tibble(finch)
# 1.7
unique(finch$year)
# 1.8
distinct(finch, year, species)

# 2 ####
# 2.1
plot(finch$blength)
# 2.2 Finches appear to be in two distinct groups related to beak size. They are either in the 8-12 mm range or the 12-16 mm range. Data entries 100-250 and 550-600 must have some variable that is causing this.
# 2.3
plot(finch$species, finch$blength)
# 2.4 the beak length is in two distinct groups because of the different species. Fortis finches have a beak lenght of about 10-11 mm, while scanden finches typically have a beak length of 13-14 mm.
# 2.5
plot(finch$blength, finch$bdepth)
# 2.6 there seems to be a positive relationship between blength and bdepth. Although their blenghts are distinctly different, the separate species seem to bave a similar range of beak depths. 

# 3 ####
# 3.1
finch |> 
  filter(blength >= 15)
# 3.2 15 rows, scandens, 2012 and 1975
# 3.3
finch |> 
  filter(bdepth < 8, 
         species == "fortis")
# 3.4
finch |> 
  filter(blength == 10 | blength == 12)
# 3.5
finch2 <- finch |> 
  mutate(finch_year = factor(year, 
    levels = c("1975", "2012")))
as_tibble(finch2)
levels(finch2$finch_year)


# 3.6  
finch_year_means <- finch2 |>
  group_by(finch_year, species) |>
  summarize(mean_blength = mean(blength),
              sd_blength = sd(blength),
            n_blength = n())|>
  mutate(se_blength = sd_blength/sqrt(n_blength)) |>
  ungroup()

finch_year_means

# 4 ####
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
  geom_histogram(position = "identity",
                 alpha = 0.75) +
facet_wrap(~finch$year) + 
  xlab("Beak Depth (mm)") + 
  scale_fill_viridis_d(option = "magma",
                       end = 0.9) + 
  theme_bw()
#4.3
ggplot(data = finch2,
       aes(x = year,
           y = bdepth,
           fill = species)) + 
  geom_boxplot()

# 4.4
 

finch2|>
  group_by(finch_year, species)|>
  summarize(mean_bdepth = mean(bdepth),
            sd_bdepth = sd(bdepth), 
            n_bdepth = n()) |>
  mutate(se_bdepth = sd_bdepth/sqrt(n_bdepth)) |>
  ggplot(aes(x = finch_year,
             y= mean_bdepth,
             fill = species,
             ymin = mean_bdepth - se_bdepth,
             ymax = mean_bdepth + se_bdepth)) + 
  geom_col(position = "dodge") + 
  geom_errorbar( position = position_dodge(0.9),
                 width = 0.4)
# 4.5

  ggplot(data = finch2,
       aes(x = blength,
           y = bdepth,
           color = species)) + 
      geom_point(alpha = 0.6) +
        facet_wrap(~year) +
        xlab("Beak Length (mm)")+ 
        ylab("Beak Depth (mm)") + 
    scale_fill_viridis_d(option ="mako",
                         begin = 0.5) + 
    theme_dark()









