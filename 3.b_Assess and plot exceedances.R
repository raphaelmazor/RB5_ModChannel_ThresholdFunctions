library(tidyverse)

##########Load threshold table##########
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

##########Get User Inputs##########
#####Input observations#####
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

#####Input Classes#####
#Get relevant classes from user
applicable_classes<-c("Wadeable streams", #Always
                      "RFI-N", #optional
                      "CVF") #optional

applicable_classes %in% thresh_df$Class

#####Input Stringency#####
#Get stringency from user
stringency_user<-c("High")



##########Prepare data##########
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



##########Add custom thresholds##########
FindThresholds(goal=.6, index="CSCI")
thresh_df %>% filter(Response_model_form=="SCAM")
#Get Index, Goal, and Class Name
CustomThreshold<-function(goal_asci_d=NA_real_, goal_asci_h=NA_real_, goal_csci=NA_real_, class="Custom class"){
  asci_d_xdf=FindThresholds(goal=goal_asci_d, index = "ASCI_D") %>%
    transmute(Class=class,
              Class_fullname=class,
              Approach="Response",
              Response_model_form = "SCAM",
              Response_model_index= "ASCI_D",
              Response_model_goal=goal_asci_d,
              Response_model_detail=paste0("Additive model, 50% probability of attaining ASCI_D score above ",goal_asci_d,"."),
              Approach2="Response SCAM ASCI_D",
              Approach3=paste0("Response (SCAM) ASCI_D ",class),
              Stringency="Custom",
              Indicator_Type="Biostimulatory",
              Indicator=Stressor_brief,
              Threshold_value=Threshold_candidate,
              Approach4=paste0(class," - Response (SCAM, ASCI_D)")
              ) %>%
    bind_rows(
      tibble(
        Class=class,
        Class_fullname=class,
        Approach="User-specified",
        Response_model_form = NA_character_,
        Response_model_index= NA_character_,
        Response_model_goal=NA_real_,
        Response_model_detail=NA_character_,
        Approach2="User-specified",
        Approach3="User-specified",
        Stringency="Custom",
        Indicator_Type="Biointegrity",
        Indicator="ASCI_D",
        Threshold_value=goal_asci_d,
        Approach4="User-specified")
    )
  
  asci_h_xdf=FindThresholds(goal=goal_asci_h, index = "ASCI_H") %>%
    transmute(Class=class,
              Class_fullname=class,
              Approach="Response",
              Response_model_form = "SCAM",
              Response_model_index= "ASCI_H",
              Response_model_goal=goal_asci_h,
              Response_model_detail=paste0("Additive model, 50% probability of attaining ASCI_H score above ",goal_asci_h,"."),
              Approach2="Response SCAM ASCI_H",
              Approach3=paste0("Response (SCAM) ASCI_H ",class),
              Stringency="Custom",
              Indicator_Type="Biostimulatory",
              Indicator=Stressor_brief,
              Threshold_value=Threshold_candidate,
              Approach4=paste0(class," - Response (SCAM, ASCI_H)")
    ) %>%
    bind_rows(
      tibble(
        Class=class,
        Class_fullname=class,
        Approach="User-specified",
        Response_model_form = NA_character_,
        Response_model_index= NA_character_,
        Response_model_goal=NA_real_,
        Response_model_detail=NA_character_,
        Approach2="User-specified",
        Approach3="User-specified",
        Stringency="Custom",
        Indicator_Type="Biointegrity",
        Indicator="ASCI_H",
        Threshold_value=goal_asci_h,
        Approach4="User-specified")
    )
  
  csci_xdf=FindThresholds(goal=goal_asci_h, index = "CSCI") %>%
    transmute(Class=class,
              Class_fullname=class,
              Approach="Response",
              Response_model_form = "SCAM",
              Response_model_index= "CSCI",
              Response_model_goal=goal_asci_h,
              Response_model_detail=paste0("Additive model, 50% probability of attaining CSCI score above ",goal_csci,"."),
              Approach2="Response SCAM CSCI",
              Approach3=paste0("Response (SCAM) CSCI ",class),
              Stringency="Custom",
              Indicator_Type="Biostimulatory",
              Indicator=Stressor_brief,
              Threshold_value=Threshold_candidate,
              Approach4=paste0(class," - Response (SCAM, CSCI)")
    ) %>%
    bind_rows(
      tibble(
        Class=class,
        Class_fullname=class,
        Approach="User-specified",
        Response_model_form = NA_character_,
        Response_model_index= NA_character_,
        Response_model_goal=NA_real_,
        Response_model_detail=NA_character_,
        Approach2="User-specified",
        Approach3="User-specified",
        Stringency="Custom",
        Indicator_Type="Biointegrity",
        Indicator="CSCI",
        Threshold_value=goal_csci,
        Approach4="User-specified")
    )
      
  bind_rows(asci_d_xdf ,asci_h_xdf, csci_xdf)
  
}
thresh_df %>%
  bind_rows(CustomThreshold(goal_asci_d = .7,goal_asci_h=.8, goal_csci=.9)) %>%
  filter(Class=="Custom class")


