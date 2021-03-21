library(tidyverse)
library(ggplot2)

rm(Moch_pH_standards_profile)

std <- Moch_pH_standards_profile %>% 
  mutate_at(vars(pH, larva, min), factor)

print(std)



means <-
  std %>%
  select(-larva) %>% ## exclude larva
  group_by(pH, min) %>% ## group by pH
  ## now compute mean and sd:
  summarize(across(everything(), na.rm = T,
                   tibble::lst(mean = mean, sd = sd))) 

print(means, n= 21)

ggplot(data = means, aes(min, area_mean, group= pH, colour= pH)) +
  geom_line() +
  labs(x = "min", y = "area") + 
  theme_classic()
