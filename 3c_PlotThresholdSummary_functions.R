library(tidyverse)
library(ggplot2)

#Load and prep required data
thresholds_df<-read_csv("Data/mod_channel_thresholds.csv") %>%
  mutate(Indicator = factor(Indicator, levels = unique(Indicator)),
         Flagged = !is.na(Flag))


#Get user info

#Get thresholds
synthesize_thresholds_plotdata<-function(
    classes,
    indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","Chl-a"),
    stringency="Intermediate") {
  
  
  thresh_dat<-thresholds_df %>%
    filter(Stringency==stringency) %>%
    # filter(!Flagged) %>%
    filter(Class %in% c("Wadeable streams",classes)) %>%
    filter(Indicator %in% indicators) %>%
    mutate(Index = case_when(Indicator_Type=="Biointegrity"~Indicator,
                             Approach=="Response"~Response_model_index,
                             T~"Not applicable"))
  thresh_dat_sum<-thresh_dat %>%
    filter(!Flagged) %>%
    group_by(Indicator) %>%
    summarise(Threshold_mean= mean(Threshold_value, na.rm=T))
  thresholds_plotdata <- list(thresh_dat, thresh_dat_sum)
  thresholds_plotdata
}

synthesize_thresholds_plot<-function(
    classes,
    indicators = c("CSCI","ASCI_D","ASCI_H","TN","TP","Chl-a","AFDM","Chl-a"),
    stringency="Intermediate") {
  
  thresholds_plotdata <- synthesize_thresholds_plotdata(classes=classes, indicators=indicators, stringency = stringency)
  ggplot(data=thresholds_plotdata[[1]], 
         aes(x=Class, y=Threshold_value))+
    # geom_point(aes(color=Approach), position=position_jitter(height=0, width=.1))+
    stat_summary( fun.y="mean", shape=8, size=1)+
    geom_point(aes(fill=Approach, shape=Index, 
                   size=Flagged),
               position=position_dodge(width=.5))+
    
    geom_hline(data=thresholds_plotdata[[2]], aes(yintercept=Threshold_mean), linetype="dashed")+
    # stat_summary( fun.y="mean", shape=10)+
    facet_wrap(~Indicator, scales="free")+
    scale_shape_manual(values=c(24,25,22, 21))+
    scale_fill_brewer(palette="Set1")+
    scale_size_manual(values=c(2,1), name="Flagged?", labels=c("No","Yes"))+
    theme_bw()+
    coord_flip()+
    guides(fill=guide_legend(override.aes = list(shape=21, size=2)),
           shape=guide_legend(override.aes = list(fill="gray", size=2)))+
    xlab("")+ylab("")
}

synthesize_thresholds_plot(classes=c("RFI-N","CVF","HB"))
