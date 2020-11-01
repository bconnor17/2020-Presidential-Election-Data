# Result lookup

# Libraries
library(DT)
library(tidyverse)
library(politicaldata)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This is a simple tool to look up and print presidential election results
# based on year, state, and either congressional district (CD) or county.

#Loading data
pres_results_cd <- politicaldata::pres_results_by_cd
pres_results_county <- read.csv("County Level Presidential Election Results 2000-2016.csv",
                                stringsAsFactors = FALSE)

pres_results$state_abb <- as.factor(pres_results$state_abb)

#Cleaning county level data.
pres_results_county_clean <- pres_results_county %>%
  mutate(party = replace_na(party, "other")) %>%
  spread(key = party, value = candidatevotes)%>%
  group_by(year, state_po, county) %>%
  summarize(democrat = sum(democrat, na.rm = T),
            republican = sum(republican, na.rm = T),
            green = sum(green, na.rm = T),
            other = sum(other, na.rm = T)) %>%
  mutate(total = democrat + republican +green + other,
         dem_share = democrat/total,
         rep_share = republican/total,
         abs_diff = abs(dem_share-rep_share))

#Congressional District Lookup Function
pres_cd_lookup <- function(x,y,z){
  cd_result <- pres_results_cd[ which(pres_results_cd$state_abb == x &
                                     pres_results_cd$district == y &
                                     pres_results_cd$year == z),]
  rep <- round(cd_result$rep, 4) * 100
  dem <- round(cd_result$dem, 4) * 100
  abs_diff <- round(abs(dem - rep),4)
  
paste0("In ",cd_result$year,", the Republican presidential candidate won ",
       rep,"% of the vote, and the Democratic candidate won ", 
       dem,"% of the vote, a difference of ",abs_diff,"%.", collapse = " ")
}

#County Lookup Function
pres_county_lookup <- function(x,y,z){
  county_result <- pres_results_county_clean[ which(pres_results_county_clean$state_po == x &
                                        pres_results_county_clean$county == y &
                                        pres_results_county_clean$year == z),]
  rep <- round(county_result$rep_share, 4) * 100
  dem <- round(county_result$dem_share, 4) * 100
  abs_diff <- round(abs(dem - rep),4)
  
  paste0("In ",county_result$year,", the Republican presidential candidate won ",
         rep,"% of the vote, and the Democratic candidate won ", 
         dem,"% of the vote, a difference of ",abs_diff,"%.", collapse = " ")
}

pres_cd_lookup("NJ", "11", "2016")

pres_county_lookup("WI","Waukesha","2016")


