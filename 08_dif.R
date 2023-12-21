library(tidyverse)

#Load the data 
Hospitals <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/hospdd.rds")

# Manually compute differences
# Step 1: Difference between treatment and control group BEFORE treatment
before_control_A <- Hospitals %>%
  filter(hospital >= 19 & hospital<= 46, procedure == 0) %>% 
  pull(satis)

before_treatment_A <- Hospitals %>%
  filter(hospital >= 1 & hospital<=18, procedure == 0) %>% 
  pull(satis)

diff_before_A <- before_treatment_A - before_control_A

# Step 2: Difference between treatment and control group AFTER treatment
after_control_A <- rep(0, dim(Hospitals)[1])

after_treatment_A <- Hospitals  %>%
  filter(hospital >= 1 & hospital<=18, procedure == 1) %>% 
  pull(satis)

diff_after_A <- after_treatment_A - after_control_A

# Step 3: Difference-in-differences.
diff_diff_A <- diff_after_A - diff_before_A
sprintf("Estimate: %.2f", diff_diff_A)


# Linear Regression 
lm_mh <- lm(satis ~ month + hospital, data = Hospitals)
summary(lm_mh)

lm_factor <- lm(satis ~ as.factor(month) + as.factor(hospital), data = Hospitals)
summary(lm_factor)
