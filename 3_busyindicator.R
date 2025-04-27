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


# Data has one line per incident. Transform to one line per potential responding ambulace per incident
d_busy= data %>% rename(call_time = dhAppel) %>% 
  select(pat_ID,call_time,starts_with("resource")) %>%
  pivot_longer(starts_with("resource_id"),values_to = "resource_id_potential",values_drop_na = T)
# Check busy status for each potential ambulance at call time
d_busy<- checkAmbulanceState(d_busy,data_ambulance_status)

# Pivot back to one line per incident
d_busy= d_busy%>% select(pat_ID,name,busy) %>%
  pivot_wider(names_from = "name",values_from = "busy",values_fill=0)
names(d_busy)[names(d_busy)=="resource_id1"] <- "busy_id1"
names(d_busy)[names(d_busy)=="resource_id2"] <- "busy_id2"
names(d_busy)[names(d_busy)=="resource_id3"] <- "busy_id3"
names(d_busy)[names(d_busy)=="resource_id4"] <- "busy_id4"
names(d_busy)[names(d_busy)=="resource_id5"] <- "busy_id5"

# Add busy computed variables to data
data_candidates = merge(data,d_busy,by=c("pat_ID"),all.x=T)


# Compute the busy indicator
data_candidates = data_candidates %>%
  mutate(across(starts_with("prob"), as.numeric)) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  mutate(probability_of_busy= prob1*busy_id1+prob2*busy_id2+prob3*busy_id3+prob4*busy_id4+prob5*busy_id5) 