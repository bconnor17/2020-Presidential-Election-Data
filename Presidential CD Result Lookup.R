# Result lookup

library(tidyverse)
library(politicaldata)

pres_results <- politicaldata::pres_results_by_cd
pres_results$state_abb <- as.factor(pres_results$state_abb)

pres_cd_lookup <- function(x,y,z){
  cd_result <- pres_results[ which(pres_results$state_abb == x &
                                     pres_results$district == y &
                                     pres_results$year == z),]
  rep <- round(cd_result$rep, 4) * 100
  dem <- round(cd_result$dem, 4) * 100
  
paste0("In ",cd_result$year,", the Republican presidential candidate won ",
       rep,"% of the vote, and the Democratic candidate won ", 
       dem,"% of the vote.", collapse = " ")
}

pres_cd_lookup("NC","5","2016")
