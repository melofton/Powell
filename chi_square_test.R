# Chi-square test
# Author: Mary Lofton
# Date: 06JUN24

# Purpose: run Chi square tests to assess significance of phytoplankton and 
# cyanobacteria response to water level

# load packages
library(tidyverse)
library(lubridate)

# get and wrangle data
dat <- read_csv("./Data/powell_litreview_edited2.csv") %>%
  filter(Include == "Yes") %>%
  select(cov_num, trophic_status, tp_ugL, phyto_response_wl_decrease, phyto_response_wl_increase,
         cyano_response_wl_decrease, cyano_response_wl_increase) %>%
  mutate(trophic_status = ifelse(trophic_status == "not reported" & tp_ugL <= 12, "oligotrophic", 
                                 ifelse(trophic_status == "not reported" & tp_ugL >12 & tp_ugL <=24, "mesotrophic", 
                                        ifelse(trophic_status == "not reported" & tp_ugL >24 & tp_ugL <= 70, "eutrophic", 
                                               ifelse(trophic_status == "not reported" & tp_ugL >70, "hypertrophic",
                                                      ifelse(trophic_status == "not reported" & is.na(tp_ugL),trophic_status,trophic_status)))))) %>%
  filter(!is.na(trophic_status) & !(is.na(phyto_response_wl_decrease) & is.na(phyto_response_wl_increase) & is.na(cyano_response_wl_decrease) & is.na(cyano_response_wl_increase))) %>%
  mutate(increase_decrease_mosaic = ifelse(((!is.na(phyto_response_wl_decrease) | !is.na(cyano_response_wl_decrease)) & (!is.na(phyto_response_wl_increase) | !is.na(cyano_response_wl_increase))),"both",
                                           ifelse((!is.na(phyto_response_wl_decrease) | !is.na(cyano_response_wl_decrease)),"decrease",
                                                  ifelse((!is.na(phyto_response_wl_increase) | !is.na(cyano_response_wl_increase)),"increase",NA)))) %>%
  mutate(phyto_response_wl_decrease = ifelse(is.na(phyto_response_wl_decrease),"not reported",phyto_response_wl_decrease),
         phyto_response_wl_increase = ifelse(is.na(phyto_response_wl_increase),"not reported",phyto_response_wl_increase),
         cyano_response_wl_decrease = ifelse(is.na(cyano_response_wl_decrease),"not reported",cyano_response_wl_decrease),
         cyano_response_wl_increase = ifelse(is.na(cyano_response_wl_increase),"not reported",cyano_response_wl_increase)) %>%
  mutate(trophic_status_mosaic = ifelse((grepl("oligo",trophic_status) | grepl("meso",trophic_status)),"oligo-mesotrophic","eu-hypereutrophic"),
         increase_mosaic = ifelse(increase_decrease_mosaic %in% c("increase","both"),"yes","no"),
         decrease_mosaic = ifelse(increase_decrease_mosaic %in% c("decrease","both"),"yes","no"),
         increase_phyto = ifelse((phyto_response_wl_decrease == "increase" | phyto_response_wl_increase == "increase"),"yes","no"),
         increase_cyano = ifelse((cyano_response_wl_decrease == "increase" | cyano_response_wl_increase == "increase"),"yes","no")) %>%
  mutate(trophic_status_mosaic = ifelse(grepl("meso-eutrophic",trophic_status),"eu-hypereutrophic",trophic_status_mosaic))


## From Agresti(2007) p.39

# first for phytoplankton
phyto <- dat %>%
  select(increase_decrease_mosaic, increase_phyto)
phyto_increase_wl_increase <- phyto %>%
  filter(increase_decrease_mosaic == "increase" & increase_phyto == "yes") %>%
  count(increase_phyto) %>%
  pull(n)
phyto_increase_wl_increase <- 0
phyto_no_increase_wl_increase <- phyto %>%
  filter(increase_decrease_mosaic == "increase" & increase_phyto == "no") %>%
  count(increase_phyto) %>%
  pull(n)
phyto_increase_wl_decrease <- phyto %>%
  filter(increase_decrease_mosaic == "decrease" & increase_phyto == "yes") %>%
  count(increase_phyto) %>%
  pull(n)
phyto_no_increase_wl_decrease <- phyto %>%
  filter(increase_decrease_mosaic == "decrease" & increase_phyto == "no") %>%
  count(increase_phyto) %>%
  pull(n)
phyto_increase_wl_both <- phyto %>%
  filter(increase_decrease_mosaic == "both" & increase_phyto == "yes") %>%
  count(increase_phyto) %>%
  pull(n)
phyto_no_increase_wl_both <- phyto %>%
  filter(increase_decrease_mosaic == "both" & increase_phyto == "no") %>%
  count(increase_phyto) %>%
  pull(n)
M <- as.table(rbind(c(phyto_increase_wl_increase, phyto_increase_wl_decrease, phyto_increase_wl_both), c(phyto_no_increase_wl_increase, phyto_no_increase_wl_decrease, phyto_no_increase_wl_both)))
dimnames(M) <- list(phyto_response = c("increase", "decrease or no change"),
                    water_level = c("increase","decrease", "both"))
M
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# now for cyanobacteria
cyano <- dat %>%
  select(increase_decrease_mosaic, increase_cyano)
cyano_increase_wl_increase <- cyano %>%
  filter(increase_decrease_mosaic == "increase" & increase_cyano == "yes") %>%
  count(increase_cyano) %>%
  pull(n)
cyano_no_increase_wl_increase <- cyano %>%
  filter(increase_decrease_mosaic == "increase" & increase_cyano == "no") %>%
  count(increase_cyano) %>%
  pull(n)
cyano_increase_wl_decrease <- cyano %>%
  filter(increase_decrease_mosaic == "decrease" & increase_cyano == "yes") %>%
  count(increase_cyano) %>%
  pull(n)
cyano_no_increase_wl_decrease <- cyano %>%
  filter(increase_decrease_mosaic == "decrease" & increase_cyano == "no") %>%
  count(increase_cyano) %>%
  pull(n)
cyano_increase_wl_both <- cyano %>%
  filter(increase_decrease_mosaic == "both" & increase_cyano == "yes") %>%
  count(increase_cyano) %>%
  pull(n)
cyano_no_increase_wl_both <- cyano %>%
  filter(increase_decrease_mosaic == "both" & increase_cyano == "no") %>%
  count(increase_cyano) %>%
  pull(n)
M <- as.table(rbind(c(cyano_increase_wl_increase, cyano_increase_wl_decrease, cyano_increase_wl_both), c(cyano_no_increase_wl_increase, cyano_no_increase_wl_decrease, cyano_no_increase_wl_both)))
dimnames(M) <- list(cyano_response = c("increase", "decrease or no change"),
                    water_level = c("increase","decrease", "both"))
M
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value
