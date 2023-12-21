# draw DAG for parking example

# Load packages
library(dagitty)
library(ggdag)

# create DAG from dagitty
dag_model <- 'dag {
bb="0,0,1,1"
L [location,pos="0.9,0.6"]
P [exposure,pos="0.575,0.4"]
S [outcome,pos="0.9,0.4"]
D [pos="0.7,0.2"]
C [pos="0.7,0.3"]
B [pos="0.7,0.5"]
A [pos="0.7,0.6"]
L-> S
P -> B
P-> A
P-> C
P-> D
A -> S
B -> S
C -> S
D -> S
}
'
# draw DAG
ggdag(dag_model) +
  theme_dag()+
  geom_dag_point(color = "pink") +
  geom_dag_text(color = "black") +
    geom_dag_edges(edge_color = "blue")




#part 2 of the assignment
df <- data.frame(
  follow_ups = c(8, 7, 8, 9, 10, 6, 7, 6, 8, 8, 0, 1, 2, 2, 3),
  satisfaction = c(40, 45, 47, 44, 50, 55, 60, 62, 59, 65, 70, 74, 77, 74, 80),
  subscription = c("Elite", "Elite", "Elite", "Elite", "Elite", "Premium+", "Premium+", "Premium+", "Premium+", "Premium+", "Premium", "Premium", "Premium", "Premium", "Premium")
)

# Regress satisfaction on follow_ups
model_follow_ups <- lm(satisfaction ~ follow_ups, data = df)
summary(model_follow_ups)
summary(model_follow_ups)$coefficients["follow_ups", "Pr(>|t|)"]


# Regress satisfaction on follow_ups and account for subscription
model_subscription <- lm(satisfaction ~ follow_ups + subscription, data = df)
summary(model_subscription)
summary(model_subscription)$coefficients["follow_ups", "Pr(>|t|)"]

#---
# Print coefficients for the first model (satisfaction ~ follow_ups)
#coefficients_follow_ups <- coef(model_follow_ups)
#print(coefficients_follow_ups)
#---
# Print coefficients for the second model (satisfaction ~ follow_ups + subscription)
#coefficients_subscription <- coef(model_subscription)
#print(coefficients_subscription)
#---

# Load ggplot2 library
library(ggplot2)

# Scatter plot with regression line for satisfaction ~ follow_ups
plot_follow_ups <- ggplot(df, aes(x = follow_ups, y = satisfaction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Satisfaction vs. Follow-ups",
       x = "Follow-ups",
       y = "Satisfaction")

# Scatter plot with regression line for satisfaction ~ follow_ups + subscription
plot_subscription <- ggplot(df, aes(x = follow_ups, y = satisfaction, color = subscription)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Satisfaction vs. Follow-ups by Subscription",
       x = "Follow-ups",
       y = "Satisfaction",
       color = "Subscription")

# Display the plots
print(plot_follow_ups)
print(plot_subscription)
