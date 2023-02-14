library(tidyverse)

#Load threshold table
thresh_df<-read_csv("Data/mod_channel_thresholds.csv") %>%
  mutate(Class = factor(Class, levels=c("Wadeable streams","RFI-N","RFI-S","CVF","HB","SB2","SB1","SB0","CC")),
         Approach = factor(Approach, levels=c("Reference","Best observed","Response")),
         Response_model_form = factor(Response_model_form, levels=c("LR","SCAM")),
         Response_model_index = factor(Response_model_index, levels=c("ASCI_D","ASCI_H","CSCI")),
         Approach4 = case_when(Approach!="Response"~paste0(Class," - ",Approach),
                               Approach=="Response"~paste0(Class," - ",Approach," (",Response_model_form,", ", Response_model_index,")"),
                               T~"OTHER")
  ) %>%
  arrange(Class,
          Approach,
          Response_model_form,
          Response_model_index) 


#Example user input from Elder Creek
obs_df<- tibble(
  Indicator=c("CSCI","ASCI_D","ASCI_H",
              "TN","TP","Chl-a","AFDM","% cover"),
  Observed_value = c(0.39, 0.97, 1.02,
                     0.79, 1.03, 13.26, 9.2, NA_real_)
)

#Example user input from Magpie Creek
obs_df<- tibble(
  Indicator=c("CSCI","ASCI_D","ASCI_H",
              "TN","TP","Chl-a","AFDM","% cover"),
  Observed_value = c(0.21, 0.92, 0.82,
                     0.64, 0.20, 154, 83, 41)
)

#Example user input from Pine Creek
obs_df<- tibble(
  Indicator=c("CSCI","ASCI_D","ASCI_H",
              "TN","TP","Chl-a","AFDM","% cover"),
  Observed_value = c(0.83, 0.9, 0.94,
                     0.07, 0.013, 33.3, 11, NA_real_)
)

#Get relevant classes from user
applicable_classes<-c("Wadeable streams", #Always
                      "RFI-N", #optional
                      "CVF") #optional

applicable_classes %in% thresh_df$Class

#Get stringency from user
stringency_user<-c("High")

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

my_thresh_df$Approach4<-factor(my_thresh_df$Approach4, 
                               levels = my_thresh_df$Approach4 %>%
                                 unique() %>%
                                 rev())



# my_thresh_df$Approach4<-factor(my_thresh_df$Approach4,
#                                levels=c("Wadeable streams - Reference",
#                                         "Wadeable streams - Response (LR, ASCI_D)",
#                                         "Wadeable streams - Response (LR, ASCI_H)",
#                                         "Wadeable streams - Response (LR, CSCI)",
#                                         "Wadeable streams - Response (SCAM, ASCI_D)",
#                                         "Wadeable streams - Response (SCAM, ASCI_H)"))

assessment_plot<-ggplot(data=my_thresh_df, 
       aes(x=obs_label, y=Approach4))+
  geom_tile(aes(fill=Threshold_pass), color="white")+
  geom_text(aes(label=Threshold_value))+
  facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
  # facet_grid(Indicator_Type~.,  scales="free", drop = T, space="free")+
  # scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#fb9a99","#ff7f00","#fdbf6f"), name="Threshold")+
  scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#cab2d6","#ff7f00","#fdbf6f"), name="Threshold", drop=F)+
  ylab("")+
  xlab("Indicator\n(Observed value)")+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(color="gray25"))

library(cowplot)
assessment_plot_cow<-cowplot::plot_grid(assessment_plot +
                     theme(legend.position = "none"),
                   cowplot::get_legend(assessment_plot),
                   nrow=2, rel_heights=c(1,.2))
assessment_plot_cow
ggsave(assessment_plot_cow, filename="Figures/assessment_plot_PINE.jpg", dpi=300, height=7.5, width=6.5)

# 
# bi_plot<-
#   ggplot(data=my_thresh_df %>% filter(Indicator_Type=="Biointegrity"), 
#          aes(x=obs_label, y=Approach3))+
#   geom_tile(aes(fill=Threshold_pass), color="white")+
#   geom_text(aes(label=Threshold_value))+
#   facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
#   # facet_grid(Indicator_Type~.,  scales="free", drop = T, space="free")+
#   scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#fb9a99","#ff7f00","#fdbf6f"),
#                     name="Threshold",
#                     drop=F
#   )+
#   ylab("")+
#   xlab("Indicator\n(Observed value)")
# bi_plot
# 
# bs_plot<-ggplot(data=my_thresh_df %>% filter(Indicator_Type=="Biostimulatory"), 
#        aes(x=obs_label, y=Approach3))+
#   geom_tile(aes(fill=Threshold_pass), color="white")+
#   geom_text(aes(label=Threshold_value))+
#   facet_wrap(~Indicator_Type, ncol=1, scales="free", drop = T)+
#   # facet_grid(Indicator_Type~.,  scales="free", drop = T, space="free")+
#   scale_fill_manual(values=c("#1f78b4", "#a6cee3","#e31a1c","#fb9a99","#ff7f00","#fdbf6f"),
#                     name="Threshold",
#                     drop=F
#   )+
#   ylab("")+
#   xlab("Indicator\n(Observed value)")
# 
# library(ggpubr)
# ggarrange(bi_plot, bs_plot, common.legend = T,
#           lege)
