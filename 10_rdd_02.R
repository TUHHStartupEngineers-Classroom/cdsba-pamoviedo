library(rddensity)
library(tidyverse)

Shipping <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/shipping.rds")

#Define cut-off
co <- 30

#Density Test
rddd <- rddensity(Shipping$purchase_amount, c = co)
summary(rddd)

#Visual check
rdd_plot <- rdplotdensity(rddd, Shipping$purchase_amount, plotN = 100)

