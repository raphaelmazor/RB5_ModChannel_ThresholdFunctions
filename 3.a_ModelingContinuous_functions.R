library(tidyverse)
library(ggplot2)


#Load required datasets
obs_points_df<-read_csv("Data/continuous_model_data/obs_points_df.csv") %>%
  mutate(BSPretty=factor(BSPretty, levels=c("Total N", "Total P", "Chl-a","AFDM", "% cover")))
my.models.predictions<-read_csv("Data/continuous_model_data/my.models.predictions.csv") %>%
  mutate(BSPretty=factor(BSPretty, levels=c("Total N", "Total P", "Chl-a","AFDM", "% cover")))




FindThresholds <-function(goal, 
                          index=c("CSCI","ASCI_D","ASCI_H"), 
                          stressor=c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP")
) {
  if(length(goal)>1) {
    stop("Incorrect number of goals entered.\nOnly a single goal may be assessed at this time")
  }
  if(any(!index  %in% c("CSCI","ASCI_D","ASCI_H"))) {
    stop(paste0(setdiff(index, c("CSCI","ASCI_D","ASCI_H"))," is not a valid index.\nValid indices: CSCI, ASCI_D, ASCI_H"))
  }
  if(any(!stressor  %in% c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP"))) {
    stop(paste0(setdiff(stressor,c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP"))," is not a valid stressor\nValid stressors: Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, Chlorophyll_a_mgPerm2, Ash_Free_Dry_Mass_mgPercm2, PCT_MAP"))
    # stop("Invalid stressor entered\nValid stressors: Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, Chlorophyll_a_mgPerm2, Ash_Free_Dry_Mass_mgPercm2, PCT_MAP")
  }
  # if (length(goal)>1 & length(goal)!=length(index)) {
        # stop("Incorrect number of goals entered.\nEnter a single goal for all indices, or enter one goal for each index")
  # }
  
  xdf = my.models.predictions %>%
    filter(BiostimVar %in% stressor) %>%
    filter(Index %in% index) %>%
    filter(Fit>=goal) %>% #Need to update for different goals for each index?
    group_by(BiostimVar,Index) %>%
    slice_max(BiostimValue, n=1) %>%
    ungroup() %>%
    select(Stressor=BiostimVar, 
           Stressor_brief=BSPretty,
           Threshold_candidate=BiostimValue, 
           IndexScore_predicted=Fit,
           IndexScore_predicted_se=SE) %>%
    mutate(IndexScore_predicted_l95=IndexScore_predicted-IndexScore_predicted_se*1.96,
           IndexScore_predicted_u95=IndexScore_predicted+IndexScore_predicted_se*1.96,)
  xdf
}
FindThresholds_plot(goal=0.75, index=c("CSCI"), stressor="PCT_MAP")
# FindThresholds(goal=0.75)
# FindThresholds(goal=c(.65,.6))

FindThresholds_plot<-function(goal, 
                              index=c("CSCI","ASCI_D","ASCI_H"), 
                              stressor=c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP"),
                              showobserved =T
) {
  point_color = ifelse(showobserved, "gray",NA)
  xdf= FindThresholds(goal=goal, index=index, stressor=stressor) %>%
    mutate(Threshold_candidate_rounded = case_when(Stressor %in% c("Nitrogen_Total_mgPerL","Phosphorus_as_P_mgPerL")~round(Threshold_candidate,3),
                                                   T~round(Threshold_candidate,0)))
  
  
  ggplot(data=my.models.predictions %>%
           filter(BiostimVar %in% stressor) %>%
           filter(Index %in% index), aes(x=BiostimValue, y=Fit))+
    geom_point(data=obs_points_df %>%
                 filter(BiostimVar %in% stressor) %>%
                 filter(Index %in% index),
               size=.5, aes(y=IndexScore), color=point_color)+
    geom_ribbon(aes(ymin=Fit-1.96*SE, ymax=Fit+1.96*SE), alpha=.2, fill="#39568cff")+
    geom_path( linewidth=1, color="#39568cff")+
    facet_grid(Index~BSPretty, scales="free_x")+
    xlab("")+ylab("Index score")+
    geom_hline(yintercept=goal, color="red", linetype="dashed", linewidth=1)+
    geom_vline(data=xdf, aes(xintercept=Threshold_candidate), color="red", linetype="dashed", linewidth=1)+
    theme_bw()+
    geom_label(data    = xdf,
               label.size=NA,
               mapping = aes(x = Inf, y = Inf, label = Threshold_candidate_rounded),
               hjust   = 1,
               vjust   = 1
    )
}

FindThresholds_plot(goal=c(.75))
