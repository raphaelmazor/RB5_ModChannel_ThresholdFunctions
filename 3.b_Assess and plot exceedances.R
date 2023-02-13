library(tidyverse)

thresh_df<-read_csv("Data/mod_channel_thresholds.csv")

thresh_df$Indicator %>%unique()
#Example from Elder Creek
obs_df<- tibble(
  Indicator=c("CSCI","ASCI_D","ASCI_H",
              "TN","TP","Chl-a","AFDM","% cover"),
  Observed_value = c(0.39, 0.97, 1.02,
                     0.79, 1.03, 13.26, 9.2, NA_real_)
)



#Get relevant classes from user
applicable_classes<-c("Wadeable streams", #Always
                      "CVF", #optional
                      "SB0") #optional

applicable_classes %in% thresh_df$Class

#Get stringency from user
stringency_user<-c("Intermediate")

my_thresh_df<-thresh_df %>%
  filter(Class %in%applicable_classes) %>%
  filter(Stringency %in% stringency_user) %>%
  inner_join(obs_df) %>%
  mutate(Threshold_pass = case_when(
    is.na(Observed_value)~"No data",
    is.na(Threshold_value)~"No threshold identified",
    Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & is.na(Flag) ~ "Fails",
    Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & is.na(Flag) ~ "Passes",
    Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & !is.na(Flag) ~ "Fails but flagged",
    Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & !is.na(Flag) ~ "Passes flagged",
    
    Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & is.na(Flag) ~ "Fails",
    Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & is.na(Flag) ~ "Passes",
    Indicator_Type == "Biostimulatory" & Threshold_value < Observed_value & !is.na(Flag) ~ "Fails but flagged",
    Indicator_Type == "Biostimulatory" & Threshold_value >= Observed_value & !is.na(Flag) ~ "Passes flagged",
    
    T~"Other"),
    obs_label = paste0(Indicator,"\n(",Observed_value,")")
  )
my_thresh_df$Threshold_pass<-factor(my_thresh_df$Threshold_pass,
                                    levels=c("Passes","Passes flagged",
                                             "Fails","Fails but flagged",
                                             "No threshold identified",
                                             "No data"))


# obs_axis_labels <- my_thresh_df %>%
#   mutate(obs_label = paste0(Indicator,"\n(",Observed_value,")")) %>%
#   select(obs_label, Indicator) %>%
#   unique()

ggplot(data=my_thresh_df, 
       aes(x=obs_label, y=Approach3))+
  geom_tile(aes(fill=Threshold_pass), color="white")+
  geom_text(aes(label=Threshold_value))+
  facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
  # facet_grid(Indicator_Type~.,  scales="free", drop = T, space="free")+
  scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#fb9a99","#ff7f00","#fdbf6f"),
                    name="Threshold"
  )+
  ylab("")+
  xlab("Indicator\n(Observed value)")
  # scale_x_discrete(labels=obs_axis_labels$obs_label)
