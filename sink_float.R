library(tidyverse)
library(ggplot2)

files <- list.files(path = "~/student documents/UBC/Research/chaoborus imaging/Mochlonyx experiments, spring 2021/Mochlonyx pressure and hypoxia/Analysis in fridge/pressure", pattern = "*.csv", full.names = T)

tbl <- sapply(files, read_csv, simplify=FALSE) #%>% 
  #bind_rows(.id = "id")

head(X2021_04_04_larva_1_airsat)
view(larva_6_pressure_off)

clean.dat <- X2021_04_07_larva_6_pressure_on %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   # what do the tildes do?/ learn syntax
  mutate(across(where(is.character), ~na_if(., "-"))) %>%
  mutate(across(where(is.character), as.double)) %>%
  mutate(cm_Y = cm_Y*-1) %>% # the Ys from ethovision are inverted
  mutate(Y_vel =  (cm_Y - lag(cm_Y, n= 2)) / (s - lag(s, n=2))) %>%
  mutate(X_vel =  (cm_X - lag(cm_X, n= 2)) / (s - lag(s, n=2)))
  
print(clean.dat, n= 50)

# velocity filters
clean.dat <- clean.dat %>% 
  mutate(X_vel = replace(X_vel, X_vel < -0.05, NA)) %>%
  mutate(X_vel = replace(X_vel, X_vel > 0.05, NA)) %>% ## filter for very small Velocity_X values
  mutate(Y_vel = replace(Y_vel, Y_vel < -0.04, NA)) %>%
  mutate(Y_vel = replace(Y_vel, Y_vel > 0.04, NA)) #%>%
#mutate(Velocity_Y = replace(Velocity_Y, Velocity_net > 0.01, NA))

##use the above filter to throw out dirty y velocity values based on X velocity
clean.dat$Y_vel[is.na(clean.dat$X_vel) == T] <- NA ## It would be nice if I could get this command into 
## the above pipe using something from tidyverse, but the ] <- NA seems to mess with the pipe syntax.
## And I can't find a clear mutate option for replacing values in a column based on whether the 
## corresponding value in another column is NA (or some other string/value)

clean.dat <- clean.dat %>% 
  mutate(zeroline = 0) %>%
  mutate(min = s/60) %>% # more reasonable x axis

ggplot(data = clean.dat, aes(x= min), na.rm= T) +
  geom_line(aes(y= Y_vel))+
  geom_line(aes(y= zeroline), linetype = "dashed") +
 # geom_smooth(aes(y= Y_vel_calc)) +
  scale_x_continuous(breaks = round(seq(min(clean.dat$min), max(clean.dat$min), by = 2),1)) +
  theme_classic()

head(clean.dat)
view(clean.dat)
