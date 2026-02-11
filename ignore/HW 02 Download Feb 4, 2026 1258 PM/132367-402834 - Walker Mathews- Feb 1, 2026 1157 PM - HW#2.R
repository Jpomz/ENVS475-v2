finch <- read.csv("data/galapago-finches.csv",
                  stringsAsFactors = TRUE)
library(tidyverse)
#1.1 
names(finch)

#1.2 
# year, band
#1.3
str(finch)

#1.4 species is a factor, there are 2 levels, fortis, scandens 
mode(finch $year)
#1.5 Year mode is numer
#1.6
as_tibble(finch)
#1.7
distinct(finch,year)
#1.8
distinct(finch,year,species)

#2.1
plot(finch$blength)
# 2.2 No data points look unusual all in groups
#2.3
plot(finch$species, finch$blength)
#2.4 The majority of the peak length for fortis is around 11, and around 14 for scandens
#2.5
plot(finch$blength, finch$bdepth)
#2.6, There was two groups going in diagonal patterns 9-12, and 13-15 less clumped
#3.1
finch |>
  filter(blength >= 15)
#3.2There were 15 rows, years 2012, and 1975 all Scandens 
#3.3
finch |>
  filter(species == "fortis",
        blength < 8 )
#3.3 There were none
#3.4
finch |>
  filter(blength == 10 |
           blength == 12)
#3.5
finch_year <- finch |>
  mutate(finch_year = factor (year,
                           levels = c( "1975", "2012")))
finch_year
as_tibble(finch_year)
#3.6
finch_year_means <- finch_year |>
  group_by(species, year) |>
  summarize(mean_blength = mean(blength),
            sd_blength = sd(blength),
            n_blength = n()) |>
  mutate(se_blength = sd_blength / sqrt(n_blength))

print(finch_year_means)

#4.1
ggplot(data=finch,
       aes(bdepth,
           fill=species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5)
#4.2
ggplot(data=finch,
       aes(x=bdepth,
           fill=species)) +
  geom_histogram(position = "identity",
                 alpha = 0.5) +
  facet_wrap(~year) + 
  labs(x="beak depth (mm)")+
  scale_fill_viridis_d("magma",
                       end=0.5)
#4.3
ggplot(data=finch_year,
       aes(x=year,
           y=bdepth,
           fill=species)) +
  geom_boxplot()
#4.4
#finch_year |>
  group_by(year,species) |>
  summarize(mean_depth = mean(bdepth),
            n_depth = n() |>
              mutate(se_depth = sd+depth / sqrt(n_depth))) |>
  ungroup() |>
  ggplot(aes(x= year,
      y =mean_blength,
      fill = species,
      ymin = mean_blength - se_depth,
      ymax= mean_blength + se_depth)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = position_dodge(0.9),
                width = 0.2) +
  labs( y= "Mean Beak Length (mm)") +
  scale_fill_viridis_d("mako", 
                       end = 0.5, alpha = 0.6)+
  coord_cartesian(ylim = c(6,11)) + 
  theme_bw()

  

#4.4 correct
ggplot(finch_year_means,
       aes(x=year,
           y=mean_blength,
           fill = species)) +
  geom_col(position = position_dodge (width=0.7),
           width = 0.7) +
  geom_errorbar(aes(ymin=mean_blength - se_blength,
                    ymax=mean_blength + se_blength),
                position=position_dodge(width = 0.9),
                width=0.8)+
  scale_fill_viridis_d("mako",
                       end = 0.7,
                       begin=0.2,
                       alpha = 0.6) +
  labs(x="Year",
       y="Mean beak length (mm",
       fill = "Species") +
  coord_cartesian(ylim= c(6,15)) +
  theme_bw()

#4.5
finch_year |>
  ggplot(aes(x=blength,
             y=bdepth,
             color = species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~year) +
  scale_color_viridis_d(option="magma",
                        begin = 0.3,
                        end=0.9) +
  theme_minimal() + 
  labs(x="Beak length (mm)",
       y= "Beal depth (mm)",
       color = "Species")

