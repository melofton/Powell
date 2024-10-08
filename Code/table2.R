# Table 2 data
# Author: Mary Lofton
# Date: 02JUN24

# Purpose: pull data needed to populate Table 2. Reviewed studies

# load packages
library(tidyverse)
library(lubridate)
library(rlang)

## Table 2 ----
# read in data
dat <- read_csv("./Data/litdata_cleaned.csv") %>%
  select(cov_num, res_name, country, lat_dd, long_dd, study_id, delta_water_level) %>%
  mutate(lat_dd = round(as.numeric(lat_dd),2),
         long_dd = round(as.numeric(long_dd),2)) %>%
  mutate(lat_dd = ifelse(is.na(lat_dd),"NR",lat_dd),
         long_dd = ifelse(is.na(long_dd),"NR",long_dd),
         delta_water_level = ifelse(delta_water_level %in% c(NA, "not reported"),"NR",delta_water_level))

dat2 <- read_csv("./Data/mosaic_plot_data_05SEP24.csv")

dat3 <- left_join(dat2, dat, by = c("cov_num","res_name")) %>%
  select(res_name, country, lat_dd, long_dd, trophic_status_mosaic, increase_decrease_mosaic, 
         delta_water_level, increase_phyto, increase_cyano, study_id) %>%
  rename(`Lake or Reservoir Name` = res_name,
         Country = country,
         `Latitude (decimal degrees)` = lat_dd,
         `Longitude (decimal degrees)` = long_dd,
         `Trophic Status` = trophic_status_mosaic,
         `Did water level increase or decrease?` = increase_decrease_mosaic,
         `Water level change (m)` = delta_water_level,
         `Did phytoplankton increase?` = increase_phyto,
         `Did cyanobacteria increase?` = increase_cyano,
         `Study ID` = study_id) %>%
  arrange(`Lake or Reservoir Name`)
write.csv(dat3, "./Data/Table2_08OCT24.csv", row.names = FALSE)

tab <- read_csv("./Data/Table2_08OCT24.csv")


## Previous Table 2 (now in supplement)
# read in data
dat <- read_csv("./Data/Table2Data.csv") %>%
  select("Covidence #","Study ID","Aim of study","How many waterbodies are in this study?",
         "What biotic variables were measured?") %>%
  rename(cov_num = `Covidence #`)
dat2 <- read_csv("./Data/powell_litreview_edited2.csv") %>%
  select(cov_num, mgmt_concerns, mgmt_strat) 

dat3 <- left_join(dat, dat2, by = c("cov_num")) %>%
  distinct() %>%
  rename(study_id = `Study ID`,
         aim = `Aim of study`,
         num_waterbodies = `How many waterbodies are in this study?`,
         biotic_vars = `What biotic variables were measured?`) %>%
  filter(!(study_id == "Carvalho 2022" & mgmt_concerns == "water level fluctuation")) %>%
  unite(mgmt, mgmt_concerns:mgmt_strat, sep = "; ", remove = TRUE, na.rm = TRUE) %>%
  select(study_id, aim, num_waterbodies, mgmt, biotic_vars) %>%
  arrange(study_id)
write.csv(dat3, "./Data/Table2.csv", row.names = FALSE)

tab <- read_csv("./Data/Table2.csv")