library(tidyverse)
library(dagitty)
library(ggdag)
library(estimatr)

#Load the data
App <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/rand_enc.rds")

# Define DAG
dag_model <- 'dag {
bb="0,0,1,1"
NF [avgpur,pos="0.7,0.4"]
UC [preavgpur,pos="0.8,0.6"]
ST [card,pos="0.9,0.4"]
PU [pos="0.6,0.4"]

UC-> NF
UC -> ST
NF -> ST
NF -> PU


}
'
# draw DAG
ggdag(dag_model) +
  theme_dag()+
  geom_dag_point(color = "blue") +
  geom_dag_text(color = "white") +
  geom_dag_edges(edge_color = "green")

#Compute the naive, biased estimate
lm_naive <- lm(time_spent ~ used_ftr, data = App)
summary(lm_naive)

#Correlation computation
cor_matrix <- cor(App)
print(cor_matrix)

app_corr <- App %>%
  filter(used_ftr == 1)
cor(app_corr$rand_enc,app_corr$time_spent)

#First Stage
first_stage <- lm(used_ftr~time_spent, data = App)
summary(first_stage)

# Predicted 'probabilities' from first stage
pred_fs <- predict(first_stage)

# Create table with predictions and actual decisions
pred_vs_actl <- tibble(
  pred = pred_fs,
  actl = App$used_ftr
)

# Plot predictions vs original
ggplot(pred_vs_actl, aes(x = pred, y = actl, color = as.factor(actl))) +
  geom_jitter(alpha = .5) +
  scale_color_discrete(labels = c("Control Group", "Treatment Group")) +
  theme(legend.title = element_blank())

# Second stage
second_stage <- lm(App$time_spent ~ first_stage$fitted.values)
summary(second_stage)


#Compute the IV estimate using 2SLS
model_iv <- iv_robust(time_spent ~ used_ftr | rand_enc, data = App)
summary(model_iv)
coefIV<- coef(model_iv)
print(coefIV)
coefNB<- coef(lm_naive)
print(coefNB)
