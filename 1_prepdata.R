# Maximal distance[m]. All incidents closer than this is used for prediction
rmax    = 1000

data <- df_pat_2024 %>%
  mutate(Hour = hour(dhAppel),
         Hour = factor(floor(Hour/8)*8),
         Weekday = as.factor(wday(dhAppel)),
         Month = as.numeric(month(dhAppel)),
         Date = date(dhAppel),
         Week = floor((as.numeric(Date)+3)/7))

data_ambulance_status <- df_pat_2024 %>%
  mutate(dhArriveeLieux = if_else(is.na(dhArriveeLieux), dhAppel, dhArriveeLieux )) %>%
  dplyr:: select(first_resource, dhArriveeLieux, dhFinAFF) %>%
  rename(t1 = dhArriveeLieux,
         t2 = dhFinAFF)

data$resource_id1 = rep(0,nrow(data))
data$resource_id2 = rep(0,nrow(data))
data$resource_id3 = rep(0,nrow(data))
data$resource_id4 = rep(0,nrow(data))
data$resource_id5 = rep(0,nrow(data))
data$prob1        = rep(0,nrow(data))
data$prob2        = rep(0,nrow(data))
data$prob3        = rep(0,nrow(data))
data$prob4        = rep(0,nrow(data))
data$prob5        = rep(0,nrow(data))

# initialize dataset for nn methods
data_nn <-  data

