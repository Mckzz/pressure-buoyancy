library(tidyverse)
library(ggplot2)

head(larva_1_velocities)

clean.dat <- larva_1_velocities %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   #what do the tildes do?
  mutate(across(where(is.character), ~na_if(., "-")))%>%
  mutate(across(where(is.character), as.double))

head(clean.dat)
view(clean.dat)

#velocity filter
clean.dat <- clean.dat %>% 
  mutate(V = replace(V, V < -0.11, NA)) %>%
  mutate(V = replace(V, V > 0.11, NA))

print(clean.dat, n=400)
view(clean.dat)

ggplot(data = clean.dat, aes(x= s, y= V), na.rm= T) +
         geom_line()