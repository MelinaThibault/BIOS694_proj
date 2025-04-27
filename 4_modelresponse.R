library(dplyr)
library(tidyverse)
library(bayesboot)
library(fixest)

# re-run with data_candidates_nn

data_mod <- data_candidates %>%
  mutate(ResponseTime = as.numeric(difftime(dhArriveeLieux, dhAppel, units = "mins")) ) %>%
  left_join(df_geo %>%
  dplyr:: select(pat_ID, Sector), by = "pat_ID" ) 

data_mod <- data_mod %>% mutate(ResponseTime = if_else(is.na(ResponseTime) | ResponseTime < 0.5 | 
                                                         ResponseTime > 90, 
                                                       median(data_mod$ResponseTime, na.rm = T),
                                                       ResponseTime) )


# 1. Calculate Mean Busy Probability (simple average) and its 95% CI
# ---------------------------------------------------
bayesboot_mean_prob_busy <- bayesboot(
  data = as.numeric(data_mod$probability_of_busy),
  statistic = mean,
  R = 1000
)

mean_busy <- mean(as.numeric(data_mod$probability_of_busy), na.rm = T)*100
CI_busy <- quantile(bayesboot_mean_prob_busy$V1, probs = c(0.025, 0.5, 0.975))

# 2. Fit Response Time ~ Busy Probability Model (per 10%) and Delay per Incident
# ---------------------------------------------------
data_mod2 <- data_mod %>%
  mutate(probability_of_busy_per10pst = probability_of_busy * 10) %>%
  drop_na()

# controlling for operational zone of Urgence Sante
bayesbootfun1 <- function(data,w) {
  fit1 <- feols(ResponseTime~Weekday+Hour+Month+probability_of_busy_per10pst | Sector,
                        data=data,
                        weights=w)
  
  delay_per_10pst    = fit1$coefficients["probability_of_busy_per10pst"]
  delay_per_incident = mean(predict(fit1,newdata = data) - 
                              predict(fit1, newdata=data %>% mutate(probability_of_busy_per10pst=0)))
  return(c(delay_per_10pst ,delay_per_incident))
}

results <- bayesboot(data =data_mod2,
                     statistic=bayesbootfun1,
                     #R=10000,
                     R = 100,
                     use.weights = T)

summary(results)
quantile(results$probability_of_busy_per10pst,c(0.025,.5,.975))
quantile(results$V1,c(0.025,.5,.975))
