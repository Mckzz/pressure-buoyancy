library(tidyverse)
library(ggplot2)

head(larva_1_pressure_on)

clean.dat <- larva_1_pressure_off %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   #what do the tildes do?
  mutate(across(where(is.character), ~na_if(., "-")))%>%
  mutate(across(where(is.character), as.double))

#velocity filter
clean.dat <- clean.dat %>% 
  mutate(Velocity_X = replace(Velocity_X, Velocity_X < -0.05, NA)) %>%
  mutate(Velocity_X = replace(Velocity_X, Velocity_X > 0.05, NA)) ## filter for very small Velocity_X values

clean.dat$Velocity_Y[is.na(clean.dat$Velocity_X) == T] <- NA ##use the above filter to throw out dirty y velocity values

## It would be nice if I could get this command into the above pipe using dplyr or something like that,
## but the ] <- NA seems to mess with the pipe syntax. And I can't find a clear mutate option 
## for replacing values in a column with NA based on whether the corresponding value in another column is NA

clean.dat <- clean.dat %>% 
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y < -0.015, NA)) %>%
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y > 0.015, NA)) %>%
  mutate(zeroline = 0)
  #mutate(Velocity_Y = replace(Velocity_Y, Velocity_net > 0.01, NA))

head(clean.dat)

#view(clean.dat)

# filter for net velocity as well?

ggplot(data = clean.dat, aes(x= s), na.rm= T) +
  geom_line(aes(y= Velocity_Y))+
  geom_line(aes(y= zeroline), linetype = "dashed") +
  theme_classic()


#### The subsequent net velocity seems to be much more reliably close to  
#### the calculated y velocity than than the simultaneous net velocity...
################ Its because I calculated using subsequent minus current, 
################ not current minus previous


print(clean.dat, n=100)
view(clean.dat)


