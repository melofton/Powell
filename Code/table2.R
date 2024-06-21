# Table 2 data
# Author: Mary Lofton
# Date: 02JUN24

# Purpose: pull data needed to populate Table 2. Reviewed studies

# load packages
library(tidyverse)
library(lubridate)
library(rlang)

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
  select(study_id, aim, num_waterbodies, mgmt, biotic_vars)
write.csv(dat3, "./Data/Table2.csv", row.names = FALSE)

tab <- read_csv("./Data/Table2.csv")