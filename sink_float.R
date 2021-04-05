library(tidyverse)
library(ggplot2)

head(X2021_04_04_larva_1_pressure_on)
view(larva_6_pressure_off)

clean.dat <- X2021_04_04_larva_1_pressure_off %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   #what do the tildes do?/ learn syntax
  mutate(across(where(is.character), ~na_if(., "-"))) %>%
  mutate(across(where(is.character), as.double)) %>%
  mutate(cm_Y = cm_Y*-1) %>% #just for consistency in the df
  mutate(Velocity_Y = Velocity_Y*-1)

head(clean.dat)

# X velocity filter
clean.dat <- clean.dat %>% 
  mutate(Velocity_X = replace(Velocity_X, Velocity_X < -0.05, NA)) %>%
  mutate(Velocity_X = replace(Velocity_X, Velocity_X > 0.05, NA)) ## filter for very small Velocity_X values

clean.dat$Velocity_Y[is.na(clean.dat$Velocity_X) == T] <- NA ##use the above filter to throw out dirty y velocity values.
## It would be nice if I could get this command into the above pipe using dplyr et al, but the ] <- NA seems to mess
## with the pipe syntax. And I can't find a clear mutate option for replacing values in a column 
## based on whether the corresponding value in another column is NA (or some other string/value)

clean.dat <- clean.dat %>% 
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y < -0.045, NA)) %>%
  mutate(Velocity_Y = replace(Velocity_Y, Velocity_Y > 0.045, NA)) %>% # Y velocity filter
  mutate(zeroline = 0) %>%
  mutate(min = s/60) # more reasonable x axis
  #mutate(Velocity_Y = replace(Velocity_Y, Velocity_net > 0.01, NA))

head(clean.dat)



# filter for net velocity as well?

ggplot(data = clean.dat, aes(x= min), na.rm= T) +
  geom_line(aes(y= Velocity_Y))+
  geom_line(aes(y= zeroline), linetype = "dashed") +
  scale_x_continuous(breaks = round(seq(min(clean.dat$min), max(clean.dat$min), by = 5),1)) +
  theme_classic()


view(clean.dat)
