library(tidyverse)
library(ggplot2)

head(larva_1_pressure_on)

clean.dat <- larva_2_pressure_off %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   #what do the tildes do?
  mutate(across(where(is.character), ~na_if(., "-")))%>%
  mutate(across(where(is.character), as.double))

head(clean.dat)

view(clean.dat)

#velocity filter
clean.dat <- clean.dat %>% 
  mutate(Velocity_X = replace(Velocity_X, Velocity_X < -0.02, NA)) %>%
  mutate(Velocity_X = replace(Velocity_X, Velocity_X > 0.02, NA)) %>% 
  mutate(Velocity_Y = na_if(Velocity_X, NA)) %>% 
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y < -0.01, NA)) %>%
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y > 0.01, NA))

# filter for net velocity as well?

ggplot(data = clean.dat, aes(x= s, y= Velocity_Y), na.rm= T) +
  geom_line()


#### The subsequent net velocity seems to be much more reliably close to  
#### the calculated y velocity than than the simultaneous net velocity...
################ Its because I calculated using subsequent minus current, 
################ not current minus previous


print(clean.dat, n=100)
view(clean.dat)


