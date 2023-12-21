library(dagitty)
library(ggdag)
library(ggplot2)
library(MatchIt)
library(tidyverse)
# Load the data set
membership_data <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/membership.rds")

# Define DAG
dag_model <- 'dag {
bb="0,0,1,1"
AP [avgpur,pos="0.9,0.6"]
PAP [preavgpur,pos="0.6,0.4"]
C [card,pos="0.9,0.4"]
A [pos="0.7,0.6"]
S [pos="0.7,0.2"]

C-> AP
PAP -> C
A-> C
S-> C
A-> PAP
S -> PAP

}
'
# draw DAG
ggdag(dag_model) +
  theme_dag()+
  geom_dag_point(color = "pink") +
  geom_dag_text(color = "blue") +
  geom_dag_edges(edge_color = "red")

#Naive Estimation
lm_model <- lm(avg_purch ~  card, data = membership_data)
summary(lm_model)
coefficients <- coef(lm_model)
print(coefficients)

#CEM
cem <- matchit(card ~ avg_purch + age + pre_avg_purch + sex,
               data = membership_data, 
               method = 'cem', 
               estimand = 'ATE')
df_cem <- match.data(cem)
model_cem<- lm(avg_purch ~ card, data = df_cem, weights = weights)
summary (model_cem)


#Nearest Neighbor
nn <- matchit(card ~ avg_purch + pre_avg_purch + sex + age,
              data = membership_data,
              method = "nearest", # changed
              distance = "mahalanobis", # changed
              replace = T)
df_nn <- match.data(nn)
nn_model <- lm(avg_purch ~ card, data = df_nn, weights = weights)
summary(nn_model)

# Inverse Probability Weighting
model_prop <- glm(card ~ avg_purch + sex + age + pre_avg_purch,
                  data=membership_data,
                  family = binomial(link="logit"))
df_aug <- membership_data %>% mutate(propensity = predict(model_prop, type = "response"))

#Extend data by IPW scores:
df_ipw <- df_aug %>% mutate(
  ipw = (card/propensity) + ((1-card) / (1-propensity)))

#Estimation
model_ipw <- lm(avg_purch ~ card,
                data = df_ipw, 
                weights = ipw)
summary(model_ipw)
