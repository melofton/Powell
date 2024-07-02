library(janitor)
library(dplyr)
library(tidyverse)

litdata <- read.csv("~/GitHubRepos/CareyLabVT/Powell/Data/litreview_official.csv")
litdata <- clean_names(litdata) #clean up column names
head(litdata)
#keep only the data we want to include
#excluded data is either a duplicate record from a multiple waterbodies study or does not meet our depth criteria
litdata <- litdata %>% 
  #select(all_of(coltokeep)) %>% 
  filter(include == "Yes") %>% 
  select(-include)

#create a combined trophic status column
litdata1 <- litdata %>% 
  mutate(trophic_status = ifelse(trophic_status == "eutophic", "eutrophic", trophic_status)) %>% 
  mutate(trophic_status = ifelse(trophic_status == "", "not reported", trophic_status)) %>% 
  mutate(trophic_status = ifelse(trophic_status == "eutophic-hypertrophic", "hypertrophic", trophic_status)) %>% #change eutrophic-hypereutrophic to hypertrophic
  mutate(trophic_status = ifelse(trophic_status == "hypereutrophic; eutrophic", "hypertrophic", trophic_status)) %>%
  mutate(mean_chlorophyll_concentration_units = ifelse(mean_chlorophyll_concentration_units == "2mg/L at surface", "not reported", mean_chlorophyll_concentration_units)) %>% #take out 2 g/L, could not find in the paper when I double-checked
  separate(mean_chlorophyll_concentration_units, sep = " |-|m", into = c("chla_ugL", "unit")) %>%
  mutate(chla_ugL = ifelse(chla_ugL == "not",NA,chla_ugL)) %>%
  mutate(chla_ugL = ifelse(chla_ugL == "",NA,chla_ugL)) %>%
  mutate(chla_ugL = as.numeric(chla_ugL)) %>% 
  mutate(trophic_status_3 = ifelse(trophic_status == "not reported" & chla_ugL <= 2.6, "oligotrophic", 
                                   ifelse(trophic_status == "not reported" & chla_ugL >2.6 & chla_ugL <=7.3, "mesotrophic", 
                                          ifelse(trophic_status == "not reported" & chla_ugL >7.3 & chla_ugL <= 56, "eutrophic", 
                                                 ifelse(trophic_status == "not reported" & chla_ugL >56, "hypertrophic",
                                                        ifelse(trophic_status == "not reported" & is.na(chla_ugL), trophic_status, trophic_status)))))) %>% 
  mutate(trophic_status_new = ifelse(tp_ug_l <= 12, "oligotrophic", 
                                     ifelse(tp_ug_l >=12 & tp_ug_l <=24, "mesotrophic", 
                                            ifelse(tp_ug_l >24 & tp_ug_l <= 70, "eutrophic", 
                                                   ifelse(tp_ug_l >70, "hypertrophic",
                                                          ifelse(is.na(tp_ug_l), trophic_status, NA)))))) %>%  
  mutate(trophic_status_combined = ifelse(trophic_status == "not reported" & !is.na(trophic_status_new), trophic_status_new,
                                          ifelse(trophic_status == "not reported" & !is.na(trophic_status_3), trophic_status_3, trophic_status))) %>% 
  select(-c(trophic_status_new, trophic_status_3, trophic_status, unit))

#overwrite a few instances for increase/decrease in phytoplankton or cyanobacteria
litdata2 <- litdata1 %>% 
  #select(cov_num, study_id, phyto_response_wl_decrease, phyto_response_wl_increase, cyano_response_wl_decrease, cyano_response_wl_increase) %>% 
  mutate(cyano_response_wl_decrease = ifelse(cyano_response_wl_decrease == "shift" & cov_num == 63, "increase", cyano_response_wl_decrease)) %>% #"higher cyano biomass during drought"
  mutate(cyano_response_wl_decrease = ifelse(cyano_response_wl_decrease == "shift" & study_id == "Camara 2015", "increase", cyano_response_wl_decrease)) %>% #"higher cyano biomass during drought"
  mutate(cyano_response_wl_increase = ifelse(cyano_response_wl_increase == "shift" & study_id == "Camara 2015", "decrease", cyano_response_wl_increase)) #"lower cyano biomass after increased water level"

litdata_cleaned <- litdata2

write.csv(litdata_cleaned, "~/GitHubRepos/CareyLabVT/Powell/Data/litdata_cleaned.csv")  


