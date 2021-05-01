library(tidyverse)
library(ggplot2)


head(X2021_04_06_larva_3_pressure_off)
view(larva_6_pressure_off)

clean.dat <- X2021_04_06_larva_3_pressure_off %>% 
  mutate(across(where(is.character), ~na_if(., "#VALUE!"))) %>%   # cleaning up excel missing value stuff. What do the tildes do?
  mutate(across(where(is.character), ~na_if(., "-"))) %>%
  mutate(across(where(is.character), as.double)) %>% #in this case everything is mumeric, but maybe wont be in a combined dataset
  mutate(cm_Y = cm_Y*-1) %>% # the Ys from ethovision are inverted
  mutate(Y_vel =  (cm_Y - lag(cm_Y, n= 2)) / (s - lag(s, n=2))) %>% #velocity from distance over time
  mutate(X_vel =  (cm_X - lag(cm_X, n= 2)) / (s - lag(s, n=2))) %>% # I only want the Y velocities, but knowing this helps filter out flicking motions
  mutate(zeroline = 0) %>%
  mutate(min = s/60)

# velocity filters based on observed and reasonable floating and sinking rates
clean.dat <- clean.dat %>% 
  mutate(X_vel = replace(X_vel, X_vel < -0.08, NA)) %>%
  mutate(X_vel = replace(X_vel, X_vel > 0.08, NA)) %>% ## filter for very small Velocity_X values
  mutate(Y_vel = replace(Y_vel, Y_vel < -0.04, NA)) %>%
  mutate(Y_vel = replace(Y_vel, Y_vel > 0.04, NA)) 

## use the above filter to throw out dirty y velocity values based on X velocity
clean.dat$Y_vel[is.na(clean.dat$X_vel) == T] <- NA ## It would be nice if I could get this command into 
## the above pipe using something from tidyverse, but the ] <- NA seems to mess with the pipe syntax.
## And, I can't find a clear mutate option for replacing values (in this case with NA) in a column based on whether the 
## corresponding value in another column is NA (or some other string/value)

# Rolling average to smooth things out because there are always points that get through the filter 
# which are way off and most likely due to the larva flicking its self and not because of changes in buoyancy 
# (I know there are better ways but this is the one I coud get to work)
smoothed <- clean.dat %>%
  mutate(lag1=lag(Y_vel),
         lag2=lag(Y_vel,2),
         lag3=lag(Y_vel,3),
         lag4=lag(Y_vel,4),
         lag5=lag(Y_vel,5),
         lag6=lag(Y_vel,6),
         lag7=lag(Y_vel,7),
         lag8=lag(Y_vel,8),
         lag9=lag(Y_vel,9),
         lag10=lag(Y_vel,10),
         lag11=lag(Y_vel,11),
         lag12=lag(Y_vel,12),
         lag13=lag(Y_vel,13),
         lag14=lag(Y_vel,14),
         lag15=lag(Y_vel,15),
         lag16=lag(Y_vel,16),
         lag17=lag(Y_vel,17),
         lag18=lag(Y_vel,18),
         lag19=lag(Y_vel,19),
         lag20=lag(Y_vel,20),
         movave=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+
                   lag13+lag14+lag15+lag16+lag17+lag18+lag19+lag20)/20)

ggplot(data = smoothed, aes(x= min), na.rm= T) +
  geom_line(aes(y= movave))+
  geom_line(aes(y= zeroline), linetype = "dashed") +
  # geom_smooth(aes(y= Y_vel_calc)) +
  xlab("Minutes") +
  ylab("vertical velocity") +
  scale_x_continuous(breaks = round(seq(min(clean.dat$min), max(clean.dat$min), by = 5),1)) +
  theme_classic()

#plot unsmoothed with crazy spikes
ggplot(data = clean.dat, aes(x= min), na.rm= T) +
  geom_line(aes(y= Y_vel))+
  geom_line(aes(y= zeroline), linetype = "dashed") +
 # geom_smooth(aes(y= Y_vel_calc)) +
  xlab("Minutes") +
  ylab("vertical velocity") +
  scale_x_continuous(breaks = round(seq(min(clean.dat$min), max(clean.dat$min), by = 5),1)) +
  theme_classic()

head(clean.dat)
view(clean.dat)
