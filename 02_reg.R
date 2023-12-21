library(tidyverse)
car <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/car_prices.rds")
dim_car <- dim(car)
# Show first lines
head(car)
summary(car)
glimpse(car)

lm_all<- lm(price~., data = car)
summary(lm_all)


lm_imp<- lm(price~enginetype+enginesize+peakrpm+stroke, data=car)
summary(lm_imp)

lm_reg<- lm(price~enginesize, data=car)
summary(lm_reg)
min(car$enginesize)
max(car$enginesize)

ncar<- car %>% mutate(seat_heating = TRUE)
lm_final <- lm(price~., data=ncar)
summary(lm_final)
