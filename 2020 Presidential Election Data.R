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

#Looking at PA movement
clintonPA <- pollavg2016 %>%
  filter(state == "Pennsylvania") %>%
  select(daystillelection, clinton)

bidenPA <- pollavg2020 %>%
  filter(state == "Pennsylvania") %>%
  select(daystillelection, biden)

clinton_bidenPA <- merge(clintonPA, bidenPA,
                         all.x = TRUE)

clinton_bidenPAplot <- ggplot(clinton_bidenPA, aes(x = daystillelection)) +
  theme_bw() +
  geom_line(aes(y = clinton), color = "darkred") +
  geom_line(aes(y = biden), color ="steelblue") +
  scale_x_reverse(lim = c(250,0)) +
  ylab("Democratic share, polling average") +
  xlab("Days until election") +
  labs(title = "FiveThirtyEight Polling Average, 2016 vs. 2020",
       subtitle = "Pennsylvania") +
  theme(legend.position = "right")

clinton_bidenPAplot
