# Mosaic plot
# Author: Mary Lofton
# Date: 02JUN24

# Purpose: make a mosaic plot to show the number of studies that had increases in 
# cyanobacteria and phytoplankton grouped by: high/low water level fluctuation and
# trophic state

# load packages
library(tidyverse)
library(lubridate)
library(ggmosaic)

# get data
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

mosaic_phyto <- ggplot(data = dat) +
  geom_mosaic(aes(x=product(increase_phyto, trophic_status_mosaic, increase_decrease_mosaic), fill = increase_decrease_mosaic, alpha = increase_phyto)) + 
  scale_alpha_manual(values =c(.5,.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="Trophic status", x="Water level increase or decrease", title = "Phytoplankton response to water level fluctuation",
       fill = "Did water level increase, decrease, or both?",
       alpha = "Did phytoplankton increase?")+
  scale_x_productlist(labels=c("no:both" = "WL fluctuation: \n no phyto increase", "yes:both" = "WL fluctuation: \n phyto increase",
                               "no:decrease" = "WL decrease: \n no phyto increase", "yes:decrease" = "WL decrease: \n phyto increase",
                               "no:increase" = "WL increase: \n no phyto increase", "yes:increase" = "WL increase: \n phyto increase"),
                      expand = c(0,0))+
  scale_y_productlist(expand = c(0,0))+
  theme_classic()+
  theme(axis.line.x.bottom=element_line(color="white"),
        axis.line.y.left=element_line(color="white"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

mosaic_phyto
plot_dat <- ggplot_build(mosaic_phyto)$data %>%
  as.data.frame %>%
  select(label, .n)
ggsave(mosaic_phyto, filename = "./Plots/mosaic_phyto.png",dev = "png",width = 8, height = 4,
       units = "in")

mosaic_cyano <- ggplot(data = dat) +
  geom_mosaic(aes(x=product(increase_cyano, trophic_status_mosaic, increase_decrease_mosaic), fill = increase_decrease_mosaic, alpha = increase_cyano)) + 
  scale_alpha_manual(values =c(.5,.9)) +
  labs(y="Trophic status", x="", title = "Cyanobacteria response to water level fluctuation",
       fill = "Did water level increase, decrease, or both?",
       alpha = "Did cyanobacteria increase?")+
  scale_x_productlist(labels=c("no:both" = "WL fluctuation: \n no cyano increase", "yes:both" = "WL fluctuation: \n cyano increase",
                            "no:decrease" = "WL decrease: \n no cyano increase", "yes:decrease" = "WL decrease: \n cyano increase",
                            "no:increase" = "WL increase: \n no cyano increase", "yes:increase" = "WL increase: \n cyano increase"),
                      expand = c(0,0))+
  scale_y_productlist(expand = c(0,0))+
  theme_classic()+
  theme(axis.line.x.bottom=element_line(color="white"),
        axis.line.y.left=element_line(color="white"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
mosaic_cyano
plot_dat <- ggplot_build(mosaic_cyano)$data %>%
  as.data.frame %>%
  select(label, .n)
ggsave(mosaic_cyano, filename = "./Plots/mosaic_cyano.png",dev = "png",width = 8, height = 4,
       units = "in")
