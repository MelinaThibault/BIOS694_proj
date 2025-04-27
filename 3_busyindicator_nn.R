# Function that checks the state of each potential ambulance at the time of a call. 
# The ambulance state data, statedata, contains one line per mission for each ambulance, 
# including start and end time t1 and t2
checkAmbulanceState <- function(data,statedata){ 
  data         <- data[order(data$resource_id_potential),]
  data$id      <- 1:nrow(data)
  
  # Loop over all candidate ambulances in the data
  Ambulances_p <- levels(factor(data$resource_id_potential))
  id           <- c()
  busy         <- c()
  
  for(k in 1:length(Ambulances_p)){
    # identify the ambulance in state-data and incident data
    ind1    <- which(data$resource_id_potential==Ambulances_p[k])
    ind2    <- which(statedata$first_resource==Ambulances_p[k])
    
    t     <- data$call_time[ind1]
    t1    <- statedata$t1[ind2]
    t2    <- statedata$t2[ind2]
    # For each case where the ambulance could be considered, check the state of that ambulance
    # X0 is T if the ambulance was busy with any mission recorded in the state-data, F otherwise
    X0    <- rep(0,length(ind1))
    for(j in 1:length(ind1)){
      X0[j] <- max(t[j]>t1&t[j]<t2,na.rm=T)
    }
    # Store results
    busy  <- c(busy,X0)
    id    <- c(id,data$id[ind1])
    
  }
  # Merge results into the data
  data       <- merge(data,data.frame(id,busy),by="id",all.x=T)
  data$busy  <- as.numeric(data$busy>0)
  
  return(data)
}
d_busy <- amb_candidates %>%
  # bring in the call timestamp
  left_join(
    data_nn %>% select(pat_ID, dhAppel),
    by = "pat_ID"
  ) %>%
  rename(call_time = dhAppel) %>%
  checkAmbulanceState(., data_ambulance_status)

d_busy <-  d_busy %>%
  select(pat_ID, rank, prob, busy) %>%
  pivot_wider(
    names_from  = rank,
    values_from = c(prob, busy),
    names_sep   = ""
  ) %>%
  rename_with(~ paste0("prob",  1:5), starts_with("prob")) %>%
  rename_with(~ paste0("busy_id",1:5), starts_with("busy"))

data_candidates_nn <- data_nn %>%
  left_join(d_busy, by = "pat_ID") %>%
  mutate(
    across(starts_with("prob"),  as.numeric),
    across(starts_with("busy_id"), ~ replace_na(., 0)),
    probability_of_busy = 
      prob1*busy_id1 + prob2*busy_id2 +
      prob3*busy_id3 + prob4*busy_id4 +
      prob5*busy_id5
  )