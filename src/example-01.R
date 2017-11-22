library(ggplot2)
library(dplyr)

x <-rnorm(100)

data(mpg)
mpg <- mpg %>% mutate_if(is.character, as.factor)







