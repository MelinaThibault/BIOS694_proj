# Loops over all incidents and predicts
#
for(k in 1:nrow(data)){
  # Record place and time of incident under consideration
  indexdate = data$Date[k]
  indexweek = data$Week[k]
  indexmonth = data$Month[k]
  index_x   = data$Longitude[k]
  index_y   = data$Latitude[k]
  
  
  # Select incidents closer than rmax, not on the same date, within two years before or after incident
  data$r = sqrt((data$Longitude-index_x)^2+(data$Latitude-index_y)^2)
  d = data[data$r<rmax & 
             data$Date!=indexdate &
             abs(data$Month-indexmonth)<2,]
  
  # Among selected, consider only 5000 closest incidents
  d = d[order(d$r),]
  d = d[1:pmin(5000,nrow(d)),]
  
  # Weigth incidents closer in time higher
  d$w = 1/(1+(d$Week-indexweek)^2)
  
  # Tabulate first responders in the learning set
  tab = sort(prop.table(table(d$first_resource)))
  div = names(tab)[tab<.01] # increase cutoff
  d$first_resource[d$first_resource %in% div] <- "other"
  tab = sort(prop.table(table(d$first_resource)))
  
  # Cases according to how many first responders in learning set
  n_ress    = length(tab)
  if (n_ress==0){            
    # If there are no incidents within rmax
    resource = c("NA","NA","NA","NA","NA")
    probs    = c("NA","NA","NA","NA","NA")
  } else if (n_ress==1){      
    # If only one responding unit, this is assigned the responding with p=1
    resource =  c(d$first_resource[1],"NA","NA","NA","NA")
    probs    =  c(1,"NA","NA","NA","NA")
  } else if (n_ress==2){      
    # In case of two responders, mulitnom returns results as a logistic regression function
    multilogit  = multinom(first_resource~Month+Hour+Weekday,data= d,weights = w)
    levs        = unique(d$first_resource)
    res         = as.character(predict(multilogit,newdata=data[k,]))
    p           = predict(multilogit,newdata=data[k,],type = "probs")
    p           = ifelse(p>.5,p,1-p) 
    resource    = c(res,levs[levs!=res],"NA","NA","NA")
    probs       = c(p,1-p,"NA","NA","NA")
  } else { 
    # If more than two responders:
    multilogit  = multinom(first_resource~Hour+Weekday,data= d,weights = w)
    p           = predict(multilogit,newdata=data[k,],type = "probs")
    p           = p[order(-p)]
    resource    = names(p)[1:5]
    probs       = unname(p[1:5])
  }
  
  # Stores the results 
  data$resource_id1[k]  = resource[1]
  data$resource_id2[k]  = resource[2]
  data$resource_id3[k]  = resource[3]
  data$resource_id4[k]  = resource[4]
  data$resource_id5[k]  = resource[5]
  data$prob1[k]         = probs[1]
  data$prob2[k]         = probs[2]
  data$prob3[k]         = probs[3]
  data$prob4[k]         = probs[4]
  data$prob5[k]         = probs[5]
  
}

