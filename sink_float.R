library(tidyverse)
library(ggplot2)



rm(larva_3)


clean.dat <- X2021_03_12_larva_4 %>% 
  mutate(V = na_if(V, "#VALUE!")) %>% 
  mutate(cm = na_if(cm, "-"))

clean.dat$cm <- as.numeric(clean.dat$cm)
clean.dat$V <- as.numeric(clean.dat$V)

clean.dat <- clean.dat %>% 
  mutate(V = replace(V, V < -0.5, NA)) %>%
  mutate(V = replace(V, V > 0.5, NA))

print(clean.dat, n=400)
view(clean.dat)

ggplot(data = clean.dat, aes(x= s, y= V), na.rm= T) +
         geom_line()
       