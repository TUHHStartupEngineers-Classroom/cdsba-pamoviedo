library(tidyverse)
random_vars <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/random_vars.rds")

# Compute the expected value, variance and standard deviation for variable age
mean_age <- mean(random_vars$age)
mean_age
var_age <- var(random_vars$age)
var_age
sd_age <- sd(random_vars$age)
sd_age

#Compute the expected value, variance and standard deviation for variable income
mean_income <- mean(random_vars$income)
mean_income
var_income <- var(random_vars$income)
var_income
sd_income <- sd(random_vars$income)
sd_income

#Compute the covariance and correlation between both variables
covariance <- cov(random_vars$age,random_vars$income)
covariance
correlation <- cor(random_vars$age,random_vars$income)
correlation

cev1<- filter(random_vars,age<=18)
mean_cev1 <- mean(cev1$income)
mean_cev1
cev2<- filter(random_vars,age>=18&age<65)
mean_cev2 <- mean(cev2$income)
mean_cev2
cev3<- filter(random_vars,age>=65)
mean_cev3<- mean(cev3$income)
mean_cev3
