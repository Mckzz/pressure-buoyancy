library(tidyverse)
library(ggplot2)



rm(larva_3)


clean.dat <- X2021_03_12_larva_3 %>% 
  mutate(V = na_if(V, "#VALUE!")) %>% 
  mutate(cm = na_if(cm, "-"))

clean.dat$cm <- as.numeric(clean.dat$cm)
clean.dat$V <- as.numeric(clean.dat$V)

  
print(clean.dat, n= 220)
view(clean.dat)


ggplot(data = clean.dat, aes(x= s) +
         geom_line(y= V)
       