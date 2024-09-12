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

# get data
dat <- read_csv("./Data/litdata_cleaned.csv") %>% 
  select(cov_num, res_name, trophic_status_combined, tp_ug_l, phyto_response_wl_decrease, phyto_response_wl_increase,
         cyano_response_wl_decrease, cyano_response_wl_increase, chla_ugL) %>%
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
  mutate(trophic_status_mosaic = ifelse((grepl("oligo",trophic_status_combined) | grepl("meso",trophic_status_combined)),"oligo/mesotrophic",
                                        ifelse(grepl("eu", trophic_status_combined),"eu/hypereutrophic","not reported")),
         increase_phyto = ifelse((increase_decrease_mosaic == "increase" & phyto_response_wl_increase == "increase"),"yes",
                                 ifelse(increase_decrease_mosaic == "increase" & phyto_response_wl_increase == "not reported","not reported",
                                        ifelse(increase_decrease_mosaic == "decrease" & phyto_response_wl_decrease == "increase","yes",
                                               ifelse(increase_decrease_mosaic == "decrease" & phyto_response_wl_decrease == "not reported","not reported","no")))),
         increase_cyano = ifelse((increase_decrease_mosaic == "increase" & cyano_response_wl_increase == "increase"),"yes",
                                 ifelse(increase_decrease_mosaic == "increase" & cyano_response_wl_increase == "not reported","not reported",
                                        ifelse(increase_decrease_mosaic == "decrease" & cyano_response_wl_decrease == "increase","yes",
                                               ifelse(increase_decrease_mosaic == "decrease" & cyano_response_wl_decrease == "not reported","not reported","no"))))) %>%
  select(cov_num, res_name, increase_decrease_mosaic, trophic_status_mosaic, increase_phyto, increase_cyano) %>%
  mutate(trophic_status_mosaic = factor(trophic_status_mosaic, levels = c("not reported","eu/hypereutrophic","oligo/mesotrophic")),
         increase_phyto = factor(increase_phyto, levels = c("yes","no","not reported")),
         increase_cyano = factor(increase_cyano, levels = c("yes","no","not reported"))) 

write.csv(dat2,"./Data/mosaic_plot_data_12SEP24.csv", row.names = FALSE)

dat_phyto <- dat2 %>%
  filter(!increase_phyto == "not reported") %>%
  select(trophic_status_mosaic, increase_decrease_mosaic, increase_phyto) %>%
  mutate(x_axis_barplot = paste(increase_decrease_mosaic, increase_phyto, sep = "_")) %>%
  group_by(x_axis_barplot, trophic_status_mosaic) %>%
  summarise(count = n()) |>
  mutate(response.count = sum(count),
         ts.prop = count/sum(count)) |>
  ungroup() %>%
  mutate(response.prop = response.count/sum(count)) %>%
  ungroup() %>%
  separate(x_axis_barplot,c("increase_decrease_mosaic","increase_phyto"), remove = FALSE) %>%
  mutate(trophic_status_mosaic = factor(trophic_status_mosaic, levels = c("oligo/mesotrophic","eu/hypereutrophic","not reported")))

# New facet label names for water level/response variable
wl.labs <- c("WL decrease","WL increase")
names(wl.labs) <- unique(dat_phyto$increase_decrease_mosaic)
response.labs <- c("no phyto \n increase", "phyto \n increase", "no phyto \n increase", "phyto \n increase") 

# Calculate bar positions
w <- c(unique(dat_phyto$response.prop)[1],unique(dat_phyto$response.prop))
pos <- data.frame(pos = 0.5 * (cumsum(w) + cumsum(c(0, w[-length(w)]))),
                  x_axis_barplot = unique(dat_phyto$x_axis_barplot))

dat_phyto <- left_join(dat_phyto, pos, by = "x_axis_barplot")


barplot_phyto <- ggplot(data = dat_phyto, aes(x = pos, y = ts.prop, width = response.prop, fill = trophic_status_mosaic)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_x_continuous(breaks = unique(dat_phyto$pos), labels = response.labs, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label = paste0("n=",count)), position = position_stack(vjust = 0.5))+ # if labels are desired
  facet_wrap(~increase_decrease_mosaic, scales = "free_x",labeller = labeller(increase_decrease_mosaic = wl.labs)) +
  scale_fill_manual(values = c("#88CCEE","#117733","gray90")) +
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+  # if no spacing preferred between bars
  labs(fill = "Trophic status",title = "Phytoplankton response to water level fluctuation")
  
barplot_phyto

ggsave(barplot_phyto, filename = "./Plots/barplot_phyto.png",dev = "png",width = 7, height = 4,
       units = "in")

# cyanobacteria plot

dat_cyano <- dat2 %>%
  filter(!increase_cyano == "not reported") %>%
  select(trophic_status_mosaic, increase_decrease_mosaic, increase_cyano) %>%
  mutate(x_axis_barplot = paste(increase_decrease_mosaic, increase_cyano, sep = "_")) %>%
  group_by(x_axis_barplot, trophic_status_mosaic) %>%
  summarise(count = n()) |>
  mutate(response.count = sum(count),
         ts.prop = count/sum(count)) |>
  ungroup() %>%
  mutate(response.prop = response.count/sum(count)) %>%
  ungroup() %>%
  separate(x_axis_barplot,c("increase_decrease_mosaic","increase_phyto"), remove = FALSE) %>%
  mutate(trophic_status_mosaic = factor(trophic_status_mosaic, levels = c("oligo/mesotrophic","eu/hypereutrophic","not reported")))

# New facet label names for water level/response variable
wl.labs <- c("WL decrease","WL increase")
names(wl.labs) <- unique(dat_cyano$increase_decrease_mosaic)
response.labs <- c("no cyano \n increase", "cyano \n increase", "no cyano \n increase", "cyano \n increase") 

# Calculate bar positions
w <- c(unique(dat_cyano$response.prop),unique(dat_cyano$response.prop)[3])
pos <- data.frame(pos = 0.5 * (cumsum(w) + cumsum(c(0, w[-length(w)]))),
                  x_axis_barplot = unique(dat_cyano$x_axis_barplot))

dat_cyano <- left_join(dat_cyano, pos, by = "x_axis_barplot")


barplot_cyano <- ggplot(data = dat_cyano, aes(x = pos, y = ts.prop, width = response.prop, fill = trophic_status_mosaic)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_x_continuous(breaks = unique(dat_cyano$pos), labels = response.labs, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label = paste0("n=",count)), position = position_stack(vjust = 0.5))+ # if labels are desired
  facet_wrap(~increase_decrease_mosaic, scales = "free_x",labeller = labeller(increase_decrease_mosaic = wl.labs)) +
  scale_fill_manual(values = c("#88CCEE","#117733","gray90")) +
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+  # if no spacing preferred between bars
  labs(fill = "Trophic status",title = "Cyanobacteria response to water level fluctuation")

barplot_cyano

ggsave(barplot_cyano, filename = "./Plots/barplot_cyano.png",dev = "png",width = 7, height = 4,
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


ggplot(data = dat, aes(x = trophic_status_combined))+
  geom_bar(stat = "count")
