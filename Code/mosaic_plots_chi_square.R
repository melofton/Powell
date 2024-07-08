# Mosaic plot
# Author: Mary Lofton
# Date: 02JUN24

# Purpose: make a mosaic plot to show the number of studies that had increases in 
# cyanobacteria and phytoplankton grouped by: increase/decrease in water level and
# trophic state; then conduct Chi square tests to see if there are significant 
# differences

# load packages
library(tidyverse)
library(lubridate)
library(ggmosaic)

# get data
dat <- read_csv("./Data/powell_litreview_edited2.csv") %>% # use edited_test_litreview.csv instead?
  filter(Include == "Yes") %>% #only include studies that we've decided to include following our criteria in Covidence
  mutate(chla_original = `Mean chlorophyll concentration (units)`) %>%
  separate(`Mean chlorophyll concentration (units)`, sep = " |-|m", into = c("chla_ugL", "unit")) %>% #get chl-a values
  mutate(chla_ugL = ifelse(chla_ugL == "not",NA,
                           ifelse(chla_ugL == 2, 2000, as.numeric(chla_ugL)))) %>% #fix a few individual values of chl-a to be NA if not reported or convert from mgL to ugL
  select(cov_num, trophic_status, tp_ugL, phyto_response_wl_decrease, phyto_response_wl_increase,
         cyano_response_wl_decrease, cyano_response_wl_increase, chla_ugL) %>%
  mutate(trophic_status = ifelse(is.na(trophic_status),"not reported",trophic_status)) %>%
  mutate(trophic_status = ifelse(trophic_status == "not reported" & tp_ugL <= 12, "oligotrophic", 
                                     ifelse(trophic_status == "not reported" & tp_ugL >12 & tp_ugL <=24, "mesotrophic", 
                                            ifelse(trophic_status == "not reported" & tp_ugL >24 & tp_ugL <= 70, "eutrophic", 
                                                   ifelse(trophic_status == "not reported" & tp_ugL >70, "hypertrophic",
                                                          ifelse(trophic_status == "not reported" & is.na(tp_ugL),trophic_status,trophic_status)))))) %>%
  mutate(trophic_status = ifelse(trophic_status == "not reported" & chla_ugL <= 2.6, "oligotrophic", 
                                 ifelse(trophic_status == "not reported" & chla_ugL >2.6 & chla_ugL <=7.3, "mesotrophic", 
                                        ifelse(trophic_status == "not reported" & chla_ugL >7.3 & chla_ugL <= 56, "eutrophic", 
                                               ifelse(trophic_status == "not reported" & chla_ugL >56, "hypertrophic",
                                                      ifelse(trophic_status == "not reported" & is.na(chla_ugL),trophic_status,trophic_status)))))) %>%
  filter(!(is.na(phyto_response_wl_decrease) & is.na(phyto_response_wl_increase) & is.na(cyano_response_wl_decrease) & is.na(cyano_response_wl_increase))) %>%
  mutate(increase_decrease_mosaic = ifelse(((!is.na(phyto_response_wl_decrease) | !is.na(cyano_response_wl_decrease)) & (!is.na(phyto_response_wl_increase) | !is.na(cyano_response_wl_increase))),"both",
                                           ifelse((!is.na(phyto_response_wl_decrease) | !is.na(cyano_response_wl_decrease)),"decrease",
                                                  ifelse((!is.na(phyto_response_wl_increase) | !is.na(cyano_response_wl_increase)),"increase",NA)))) 

fluctuation_studies_keep_increase <- dat %>%
  filter(increase_decrease_mosaic == "both") %>%
  mutate(increase_decrease_mosaic = "increase",
         phyto_response_wl_decrease = NA,
         cyano_response_wl_decrease = NA) 

fluctuation_studies_keep_decrease <- dat %>%
  filter(increase_decrease_mosaic == "both") %>%
  mutate(increase_decrease_mosaic = "decrease",
         phyto_response_wl_increase = NA,
         cyano_response_wl_increase = NA) 

dat_no_fluctuation_studies <- dat %>%
  filter(!increase_decrease_mosaic == "both")

dat2 <- bind_rows(dat_no_fluctuation_studies, fluctuation_studies_keep_increase) %>%
  bind_rows(., fluctuation_studies_keep_decrease) %>%
  mutate(phyto_response_wl_decrease = ifelse(is.na(phyto_response_wl_decrease),"not reported",phyto_response_wl_decrease),
         phyto_response_wl_increase = ifelse(is.na(phyto_response_wl_increase),"not reported",phyto_response_wl_increase),
         cyano_response_wl_decrease = ifelse(is.na(cyano_response_wl_decrease),"not reported",cyano_response_wl_decrease),
         cyano_response_wl_increase = ifelse(is.na(cyano_response_wl_increase),"not reported",cyano_response_wl_increase)) %>%
  mutate(trophic_status_mosaic = ifelse((grepl("oligo",trophic_status) | grepl("meso",trophic_status)),"oligo-mesotrophic",
                                        ifelse(grepl("eu", trophic_status),"eu-hypereutrophic","not reported")),
         increase_phyto = ifelse((increase_decrease_mosaic == "increase" & phyto_response_wl_increase == "increase"),"yes",
                                 ifelse(increase_decrease_mosaic == "increase" & phyto_response_wl_increase == "not reported","not reported",
                                        ifelse(increase_decrease_mosaic == "decrease" & phyto_response_wl_decrease == "increase","yes",
                                               ifelse(increase_decrease_mosaic == "decrease" & phyto_response_wl_decrease == "not reported","not reported","no")))),
         increase_cyano = ifelse((increase_decrease_mosaic == "increase" & cyano_response_wl_increase == "increase"),"yes",
                                 ifelse(increase_decrease_mosaic == "increase" & cyano_response_wl_increase == "not reported","not reported",
                                        ifelse(increase_decrease_mosaic == "decrease" & cyano_response_wl_decrease == "increase","yes",
                                               ifelse(increase_decrease_mosaic == "decrease" & cyano_response_wl_decrease == "not reported","not reported","no"))))) %>%
  select(cov_num, increase_decrease_mosaic, trophic_status_mosaic, increase_phyto, increase_cyano) %>%
  mutate(trophic_status_mosaic = factor(trophic_status_mosaic, levels = c("not reported","eu-hypereutrophic","oligo-mesotrophic")),
         increase_phyto = factor(increase_phyto, levels = c("yes","no","not reported")),
         increase_cyano = factor(increase_cyano, levels = c("yes","no","not reported"))) 

write.csv(dat2,"./Data/mosaic_plot_data_for_Bridget.csv", row.names = FALSE)

dat_phyto <- dat2 %>%
  filter(!increase_phyto == "not reported") %>%
  select(trophic_status_mosaic, increase_decrease_mosaic, increase_phyto) %>%
  mutate(increase_phyto = factor(increase_phyto, levels = c("no","yes")))

mosaic_phyto <- ggplot(data = dat_phyto) +
  geom_mosaic(aes(x=product(increase_phyto, trophic_status_mosaic, increase_decrease_mosaic), fill = increase_decrease_mosaic, alpha = increase_phyto)) + 
  scale_alpha_manual(values =c(.3,.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="Trophic status", x="Water level increase or decrease", title = "Phytoplankton response to water level fluctuation",
       fill = "Did water level increase or decrease?",
       alpha = "Did phytoplankton increase?")+
  scale_x_productlist(labels=c("no:decrease" = "WL decrease: \n no phyto increase","yes:decrease" = "WL decrease: \n phyto increase",
                               "no:increase" = "WL increase: \n no phyto increase","yes:increase" = "WL increase: \n phyto increase"),
                      expand = c(0,0))+
  scale_y_productlist(expand = c(0,0))+
  scale_fill_manual(values = c("#88CCEE","#117733"))+
  theme_classic()+
  theme(axis.line.x.bottom=element_line(color="white"),
        axis.line.y.left=element_line(color="white"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

mosaic_phyto

plot_dat_phyto <- ggplot_build(mosaic_phyto)$data %>%
  as.data.frame %>%
  select(label, .n)

ggsave(mosaic_phyto, filename = "./Plots/mosaic_phyto.png",dev = "png",width = 8, height = 4,
       units = "in")

dat_cyano <- dat2 %>%
  filter(!increase_cyano == "not reported") %>%
  select(trophic_status_mosaic, increase_decrease_mosaic, increase_cyano) %>%
  mutate(increase_cyano = factor(increase_cyano, levels = c("no","yes")))

mosaic_cyano <- ggplot(data = dat_cyano) +
  geom_mosaic(aes(x=product(increase_cyano, trophic_status_mosaic, increase_decrease_mosaic), fill = increase_decrease_mosaic, alpha = increase_cyano)) + 
  scale_alpha_manual(values =c(.3,.9)) +
  labs(y="Trophic status", x="", title = "Cyanobacteria response to water level fluctuation",
       fill = "Did water level increase or decrease?",
       alpha = "Did cyanobacteria increase?")+
  scale_x_productlist(labels=c("no:decrease" = "WL decrease: \n no cyano increase","yes:decrease" = "WL decrease: \n cyano increase",
                               "no:increase" = "WL increase: \n no cyano increase","yes:increase" = "WL increase: \n cyano increase"),
                      expand = c(0,0))+
  scale_y_productlist(expand = c(0,0))+
  scale_fill_manual(values = c("#88CCEE","#117733"))+
  theme_classic()+
  theme(axis.line.x.bottom=element_line(color="white"),
        axis.line.y.left=element_line(color="white"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_legend(order = 1),
         alpha  = guide_legend(order = 2))

mosaic_cyano

plot_dat_cyano <- ggplot_build(mosaic_cyano)$data %>%
  as.data.frame %>%
  select(label, .n)

ggsave(mosaic_cyano, filename = "./Plots/mosaic_cyano.png",dev = "png",width = 8, height = 4,
       units = "in")

## From Agresti(2007) p.39

# first for phytoplankton
phyto <- dat2 %>%
  select(increase_decrease_mosaic, increase_phyto)
phyto_increase_wl_increase <- phyto %>%
  filter(increase_decrease_mosaic == "increase" & increase_phyto == "yes") %>%
  count(increase_phyto) %>%
  pull(n)
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

# Question 1: are there differences in phytoplankton responses to increases vs.
# decreases in water level?
M <- as.table(rbind(c(phyto_increase_wl_increase, phyto_increase_wl_decrease), c(phyto_no_increase_wl_increase, phyto_no_increase_wl_decrease)))
dimnames(M) <- list(phyto_response = c("increase", "decrease or no change"),
                    water_level = c("increase","decrease"))
M
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# Question 2: are there differences in the likelihood of phytoplankton to increase
# or decrease given an increase in water level?
M_increase <- M[,1]
M_increase
(Xsq <- chisq.test(M_increase))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# Question 3: are there differences in the likelihood of phytoplankton to increase
# or decrease given a decrease in water level?
M_decrease <- M[,2]
M_decrease
(Xsq <- chisq.test(M_decrease))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# now for cyanobacteria
cyano <- dat2 %>%
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

# Question 1: are there differences in cyanobacterial responses to increases vs.
# decreases in water level?
M <- as.table(rbind(c(cyano_increase_wl_increase, cyano_increase_wl_decrease), c(cyano_no_increase_wl_increase, cyano_no_increase_wl_decrease)))
dimnames(M) <- list(cyano_response = c("increase", "decrease or no change"),
                    water_level = c("increase","decrease"))
M
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# Question 2: are there differences in the likelihood of cyanobacteria to increase
# or decrease given an increase in water level?
M_increase <- M[,1]
M_increase
(Xsq <- chisq.test(M_increase))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

# Question 3: are there differences in the likelihood of cyanobacteria to increase
# or decrease given a decrease in water level?
M_decrease <- M[,2]
M_decrease
(Xsq <- chisq.test(M_decrease))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
Xsq$p.value

