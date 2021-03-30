library(tidyverse)
library(ggplot2)

rm(Moch_pH_standards_profile_post)

std <- Moch_pH_standards_profile_post %>% 
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

################## take only the 60 min time points

hour <-
  means %>% slice_max(min)

print(hour)

####################     Norm to 7      ######################


hour.norm7 <- hour %>%
  group_by(min) %>% # allows multiple obvs so that the ormula can work
  mutate(
    area.pct.change = 
      ((area_mean - area_mean[2]) / area_mean[2])*100) %>%
  ungroup() 

print(hour.norm7, n= 50)

#### plotting ####

hour.norm7 %>%
  ggplot(aes(x= pH, y= area.pct.change)) +
  geom_line(group= 1) +
  geom_point(pch= 19, size= 2, 
             aes(y= area.pct.change)) +
  geom_errorbar(mapping = aes(x = pH,
                              ymin = area.pct.change - area_sd,
                              ymax = area.pct.change + area_sd), 
                width = 0.05,
                size = 0.75) +
  labs(x = "pH", y = "% change") +
  labs(color="Dimension") +
  theme_classic()



