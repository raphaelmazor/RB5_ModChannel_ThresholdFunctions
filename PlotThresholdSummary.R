library(tidyverse)
library(ggplot2)

thresholds_df<-read_csv("Data/mod_channel_thresholds.csv")
thresholds_df$Indicator<-factor(thresholds_df$Indicator, levels= unique(thresholds_df$Indicator))

ggplot(data=thresholds_df %>%
         filter(Stringency=="Intermediate"), 
       aes(x=Class, y=Threshold_value))+
  # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
  geom_point(aes(color=Approach))+
  stat_summary(fun.y="mean", shape=4)+
  facet_wrap(~Indicator, scales="free")+
  # scale_color_viridis_d(begin=.2, end=.7)+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  coord_flip()


thresholds_df$Flagged<-!is.na(thresholds_df$Flag)


ggplot(data=thresholds_df %>%
         filter(Stringency=="Intermediate"), 
       aes(x=Class, y=Threshold_value))+
  # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
  geom_point(aes(color=Approach, alpha=Flagged), shape=16)+
  stat_summary(data=. %>% filter(!Flagged), fun.y="mean", shape="|")+
  # stat_summary( fun.y="mean", shape=10)+
  facet_wrap(~Indicator, scales="free")+
  # scale_color_viridis_d(begin=.2, end=.7)+
  scale_color_brewer(palette="Set1")+
  scale_alpha_manual(values=c(1,.5), labels=c("No","Yes"))+
  theme_bw()+
  coord_flip()


plot_dat<-thresholds_df %>%
  filter(Stringency=="Intermediate") %>%
  # filter(!Flagged) %>%
  filter(Class %in% c("Wadeable streams","HB","CVF")) %>%
  mutate(Index = case_when(Indicator_Type=="Biointegrity"~Indicator,
                           Approach=="Response"~Response_model_index,
                           T~"Not applicable"))
plot_dat_sum<-plot_dat %>%
  filter(!Flagged) %>%
  group_by(Indicator) %>%
  summarise(Threshold_mean= mean(Threshold_value))

ggplot(data=plot_dat, 
       aes(x=Class, y=Threshold_value))+
  # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
  geom_point(aes(color=Approach), shape=16)+
  stat_summary(data=. %>% filter(!Flagged), fun.y="mean", shape="|")+
  geom_hline(data=plot_dat_sum, aes(yintercept=Threshold_mean), linetype="dotted")+
  # stat_summary( fun.y="mean", shape=10)+
  facet_wrap(~Indicator, scales="free")+
  # scale_color_viridis_d(begin=.2, end=.7)+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  coord_flip()

ggplot(data=plot_dat, 
       aes(x=Class, y=Threshold_value))+
  # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
  geom_point(aes(fill=Approach, shape=Index, 
                 size=Flagged),
             position=position_dodge(width=.5))+
  stat_summary( fun.y="mean", shape="|", size=1)+
  geom_hline(data=plot_dat_sum, aes(yintercept=Threshold_mean), linetype="dotted")+
  # stat_summary( fun.y="mean", shape=10)+
  facet_wrap(~Indicator, scales="free")+
  scale_shape_manual(values=c(24,25,22, 21))+
  scale_fill_brewer(palette="Set1")+
  scale_size_manual(values=c(2,1), name="Flagged?", labels=c("No","Yes"))+
  theme_bw()+
  coord_flip()+
  guides(fill=guide_legend(override.aes = list(shape=21, size=2)),
         shape=guide_legend(override.aes = list(fill="gray", size=2)))
  




threshold_summary_df<-crossing(
  Geography=c("CVF","NorCal, not CVF", "SoCal"),
  FlowStatus=c("Perennial","Intermittent"),
  Modification=c("Natural","HB","SB2","SB1","SB0","CC"),
  # Indicator=c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","% cover"),
  # Stringency=c("High","Intermediate","Low")
)

threshold_summary_df$Geography %>% unique()
# threshold_summary_df$RelevantClasses<-
lapply(1:nrow(threshold_summary_df), function(i){
  geog.i<-threshold_summary_df$Geography[i]
  flow.i<-threshold_summary_df$FlowStatus[i]
  mod.i<-threshold_summary_df$Modification[i]
  print(paste(geog.i, flow.i, mod.i))
  classes<-c("Wadeable streams")
  classes<-ifelse(geog.i=="CVF",c(classes,"CVF"),classes)
  # classes<-ifelse(geog.i %in% c("CVF","NorCal, not CVF") & flow.i=="Intermittent", c(classes, "RFI-N"), classes)
  # classes<-ifelse(geog.i %in% c("SoCal") & flow.i=="Intermittent", c(classes, "RFI-S"), classes)
  classes %>% unique()
})
