ppmr <- read.csv("data/ppmr.csv")


m1 <- glm(data = ppmr,
          formula = link ~ log10_ppmr,
          family = binomial(link = "logit"))
summary(m1)
#library(tidyverse)

ggplot(ppmr,
       aes(y = link, 
           x = (log10_ppmr))) +
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, 
              method.args = list(family = binomial)) 
