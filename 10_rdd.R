library(rddensity)

coupon <- readRDS("D:/GitHub/cdsba-pamoviedo/Causal_Data_Science_Data/coupon.rds")

#define cut-off
c0 <- 60

# Define bandwidths
bw <- c0 + c(-5, 5) 
bw_1 <- c0 + c(-2.5,2,5) # Half BW
bw_2 <- c0 + c(-10,10)   # Double BW

# Normal Bandwidth:
# Subsets below and above threshold in specified bandwidth
df_bw_below <- coupon %>% filter(days_since_last >= bw[1] & days_since_last < c0)
df_bw_above <- coupon %>% filter(days_since_last >= c0 & days_since_last <= bw[2])

df_bw <- bind_rows(df_bw_above, df_bw_below)

lm_bw <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw)
summary(lm_bw)

# Case of Half the bandwidth
df_bw_below_1 <- coupon %>% filter(days_since_last >= bw_1[1] & days_since_last < c0)
df_bw_above_1 <- coupon %>% filter(days_since_last >= c0 & days_since_last <= bw_1[2])

df_bw_1 <- bind_rows(df_bw_above_1, df_bw_below_1)

lm_bw_1 <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw_1)
summary(lm_bw_1)

# Case of Double the bandwidth
df_bw_below_2 <- coupon %>% filter(days_since_last >= bw_2[1] & days_since_last < c0)
df_bw_above_2 <- coupon %>% filter(days_since_last >= c0 & days_since_last <= bw_2[2])

df_bw_2 <- bind_rows(df_bw_above_2, df_bw_below_2)

lm_bw_2 <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw_2)
summary(lm_bw_2)




