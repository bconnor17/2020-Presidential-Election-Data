#Package load
library(ggplot2)
library(tidyverse)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

eday2016 <- as.Date("2016-11-08")
eday2020 <- as.Date("2020-11-03")

#Bring in data
pollavg2020 <- read.csv("presidential_poll_averages_2020.csv", stringsAsFactors = FALSE)

pollavg19682016 <- read.csv("pres_pollaverages_1968-2016.csv", stringsAsFactors = FALSE)


#Make necessary modifications
pollavg2016 <- pollavg19682016 %>%
  select(cycle:pct_estimate, -candidate_id) %>%
  filter(cycle == "2016") %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"),
         daystillelection = eday2016 - modeldate) %>%
  spread(key = candidate_name, value = pct_estimate) %>%
  rename(clinton = `Hillary Rodham Clinton`,
         trump = `Donald Trump`,
         johnson = `Gary Johnson`) %>%
  mutate(dem_margin = clinton - trump,
         abs_margin = abs(dem_margin))


pollavg2020 <- pollavg2020 %>%
  select(-pct_trend_adjusted) %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"),
         daystillelection = eday2020 - modeldate) %>%
  spread(key = candidate_name, value = pct_estimate) %>%
  rename(biden = `Joseph R. Biden Jr.`,
         trump = `Donald Trump`) %>%
  mutate(dem_margin = biden - trump,
         abs_margin = abs(dem_margin))

pollavg2020 <- pollavg2020[-c(5:6)]

#Today's snapshot
daystillelection2020 <- as.numeric(eday2020 - Sys.Date())

pollavg2016_today <- pollavg2016 %>%
  filter(daystillelection == daystillelection2020) %>%
  arrange(abs_margin)

pollavg2020_today <- pollavg2020 %>%
  filter(daystillelection == daystillelection2020) %>%
  arrange(abs_margin)
  
#Movement until the election
