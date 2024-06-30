## Plot Webtool Scenarios

# setwd("C:/Users/bdeemer/OneDrive - DOI/Documents/Lake Powell/Cyanobacteria_SEIS_WQ/WebTool_Plots")

library(dplyr)
library(ggplot2)
library(cowplot)

Mead_STGY07847<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Mead_STGY07847.csv")
Mead_STGY24145<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Mead_STGY24145.csv")
Mead_STGY44235<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Mead_STGY44235.csv")
Mead_STGY51973<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Mead_STGY51973.csv")

Powell_STGY07847<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Powell_STGY07847.csv")
Powell_STGY24145<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Powell_STGY24145.csv")
Powell_STGY44235<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Powell_STGY44235.csv")
Powell_STGY51973<-read.csv(file="~/GitHubRepos/CareyLabVT/Powell/Data/Tool Traces/Powell_STGY51973.csv")

#Make a long file for each reservoir
Powell<-rbind(Powell_STGY07847,Powell_STGY24145,Powell_STGY44235)
#Powell<-rbind(Powell_STGY07847,Powell_STGY24145,Powell_STGY44235,Powell_STGY51973)
Powells<-Powell %>%
  mutate(match=paste(time_series_month,time_series_year,strategy_id))%>%
  group_by(match)%>%
  summarise(elev=median(time_series_value),elev_low=quantile(time_series_value,probs=0.10,na.rm=TRUE),
            elev_high=quantile(time_series_value,probs=0.90,na.rm=TRUE),strategy=strategy_id[1],datetime=time_series_date[1])

Powells$date<-as.Date(Powells$datetime,format="%Y-%m-%d")
Powells$strategy<-as.factor(Powells$strategy)

Powellplot<-
  ggplot(data=Powells,aes(x=date,y=elev,fill=strategy))+
  geom_line(aes(color=strategy))+
  scale_color_manual(values=c("#7A524B","#3C7F29","#8DBAE9","#8552CE")) +
  scale_fill_manual(values=c("#7A524B","#3C7F29","#8DBAE9","#8552CE")) +
  geom_ribbon(aes(ymin=elev_low,ymax=elev_high),alpha=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        strip.background = element_blank(),
        #legend.position="none",
        #strip.text.x = element_blank(),
        #axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),
        #legend.position="top",legend.title=element_blank(), 
        axis.text=element_text(size=36),
        axis.title=element_text(size=36))+
  ylab("Powell Elevation (ft)") + 
  xlab("Year")
Powellplot

Mead<-rbind(Mead_STGY07847,Mead_STGY24145,Mead_STGY44235)
#Mead<-rbind(Mead_STGY07847,Mead_STGY24145,Mead_STGY44235,Mead_STGY51973)

Meads<-Mead %>%
  mutate(match=paste(time_series_month,time_series_year,strategy_id))%>%
  group_by(match)%>%
  summarise(elev=median(time_series_value),elev_low=quantile(time_series_value,probs=0.10,na.rm=TRUE),
            elev_high=quantile(time_series_value,probs=0.90,na.rm=TRUE),strategy=strategy_id[1],datetime=time_series_date[1])

Meads$date<-as.Date(Meads$datetime,format="%Y-%m-%d")
Meads$strategy<-as.factor(Meads$strategy)

#3C7F29, #FF922E, #9A3324, "#8DBAE9"

Meadplot<-
  ggplot(data=Meads,aes(x=date,y=elev,fill=strategy))+
  geom_line(aes(color=strategy))+
  scale_color_manual(values=c("#7A524B","#3C7F29","#8DBAE9","#8552CE")) +
  scale_fill_manual(values=c("#7A524B","#3C7F29","#8DBAE9","#8552CE")) +
  geom_ribbon(aes(ymin=elev_low,ymax=elev_high),alpha=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.x=element_blank(),axis.title.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position="none",
        axis.text=element_text(size=36),
        axis.title=element_text(size=36))+
  ylab("Mead Elevation (ft)")+
  xlab("Year")
Meadplot

# ggsave("~/GitHubRepos/CareyLabVT/Powell/Plots/LitReview/Meadplot.png", plot = Meadplot, 
#        height = 6, width = 6.5, units = "in")
# ggsave("~/GitHubRepos/CareyLabVT/Powell/Plots/LitReview/Powellplot.png", plot = Powellplot, 
#        height = 6, width = 6.5, units = "in")
# 
# stacked<-plot_grid(Powellplot,Meadplot,ncol=1,align="v")
# stacked
# 
# ggsave("C:/Users/bdeemer/OneDrive - DOI/Documents/Lake Powell/Cyanobacteria_SEIS_WQ/WebTool_Plots/scenario_plot.jpeg",
#        stacked,height=6,width=3.5,units='in') 
