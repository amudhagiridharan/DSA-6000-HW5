func1 <- function  (testinset) {
  testinset$prob <- 0
  for ( v in 1 : nrow(testinset))
  {
    uniquecarriercoeff =0;
    if (testinset[v,]$UniqueCarrier == 'DL')
    {uniquecarriercoeff = -0.79748 }
    if (testinset[v,]$UniqueCarrier == 'UA')
    {uniquecarriercoeff = -0.2137 }
    prob = exp(-0.9950+
                 -0.11297 * as.numeric(testinset[v,]$Month)+
                 0.016554   * as.numeric(testinset[v,]$SchedElapsedTime)+
                 -2.480e-03   * as.numeric(testinset[v,]$Distance)+
                 uniquecarriercoeff)/(1+ exp(-0.9950+
                                               -0.11297 * as.numeric(testinset[v,]$Month)+
                                               0.016554   * as.numeric(testinset[v,]$SchedElapsedTime)+
                                               -2.480e-03   * as.numeric(testinset[v,]$Distance)+
                                               uniquecarriercoeff))
    testinset[v,]$prob <-prob
    # cat(paste0("prob : " , prob, "cancellation value: ",testinset[v,]$Canceled , "\n"))
  }
  return (testinset$prob)
}

func2 <-function(testinset){
  score <- func1(testinset)
  label <- ifelse(score > 0.22 , 1, 0)
  return(label)
}
