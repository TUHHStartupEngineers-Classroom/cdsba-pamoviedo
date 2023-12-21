# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the dataset
abtest_data <-  readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/abtest_online.rds")

lm_ate <-lm(purchase_amount ~ chatbot, data= abtest_data)
summary(lm_ate)

compare_prev <- ggplot(abtest_data,
                       aes(x=chatbot,
                           y=previous_visit,
                           color = as.factor(chatbot)))  + 
  stat_summary(geom = "errorbar", 
               width = .5,
               fun.data = "mean_se", 
               fun.args = list(mult=1.96),
               show.legend = F) +
  labs(x = NULL, y = "Previous_visits", title = "Difference in previous visits")


compare_mobile <- ggplot(abtest_data,
                         aes(x=chatbot,
                             y=mobile_device,
                             color = as.factor(chatbot)))  + 
  stat_summary(geom = "errorbar", 
               width = .5,
               fun.data = "mean_se", 
               fun.args = list(mult=1.96),
               show.legend = F) +
  labs(x = NULL, y = "Mobile decives", title = "Difference in mobile devices")


compare_outcome <-  ggplot(abtest_data, 
                           aes(x = chatbot, 
                               y = purchase_amount, 
                               color = as.factor(chatbot))) +
  stat_summary(geom = "errorbar", 
               width = .5,
               fun.data = "mean_se", 
               fun.args = list(mult=1.96),
               show.legend = F) +
  labs(x = NULL, y = "Outcome", title = "Difference in outcome")




plot(compare_prev)
plot (compare_mobile) 
plot(compare_outcome)
#--------
#2nd part
lm_all <- lm(purchase_amount~chatbot+previous_visit+mobile_device, data=abtest_data)
summary(lm_all)
#3rd part
# Include an interaction term
lm_model <- lm(purchase_amount ~ chatbot * mobile_device, data = abtest_data)
summary(lm_model)
#4th part
#Logistic regression
logistic_model <- glm(purchase ~ chatbot + mobile_device + previous_visit, 
                      data = abtest_data, 
                      family = binomial(link = 'logit'))
summary(logistic_model)

# Extract coefficient for chatbot
chatbot_coefficient <- coef(logistic_model)["chatbotTRUE"]
# Calculate odds ratio
odds_ratio <- exp(chatbot_coefficient)
