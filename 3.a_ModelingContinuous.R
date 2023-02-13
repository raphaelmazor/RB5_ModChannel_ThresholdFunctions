library(tidyverse)
library(ggplot2)

mydf.c<-read_csv("NotForGit/1_DataPrep/mydf.c.csv") %>%
  mutate(PSA2=case_when(PSA6c %in% c("NC","SN")~"Wet",T~"Arid"))
asci.df.c<-read_csv("NotForGit/1_DataPrep/asci.df.c.csv") %>%
  mutate(PSA2=case_when(PSA6c %in% c("NC","SN")~"Wet",T~"Arid"))

#Create vectors of modeling variables
#Biointegrity variables
response.varz<-c("CSCI","ASCI_D", "ASCI_H") #"ASCI_S",
#Biostiulatory variables
chem.varz<-c("Nitrogen_Total_mgPerL","Phosphorus_as_P_mgPerL",
             # "Nitrate_as_N_mgPerL","Nitrite_as_N_mgPerL","Nitrate_Nitrite_as_N_mgPerL","OrthoPhosphate_as_P_mgPerL","Ammonia_as_N_mgPerL",
             NULL)
om.varz<-c(  "Chlorophyll_a_mgPerm2","Ash_Free_Dry_Mass_mgPercm2",
             "PCT_MAP",
             NULL)
bs.varz<-c(chem.varz, om.varz)
bs.varz_pretty<-c("Total N", "Total P", "Chl-a", "AFDM", "% cover") #This is for graphs and plots
bs.varz.df<-data.frame(BiostimVar=bs.varz, BSPretty=bs.varz_pretty, stringsAsFactors = F)

#MMI thresholds:
asci_d_thresh<-c(.94,.86,.75)  #qnorm(mean=1, sd=0.17, p=c(0.3,0.1,.01))
# asci_s_thresh<-c(.86,.65,.37)  #qnorm(mean=1, sd=0.14, p=c(0.3,0.1,.01))
asci_h_thresh<-c(.94,.86,.75)  #qnorm(mean=1, sd=0.13, p=c(0.3,0.1,.01))

thresholds.df<-data.frame(Index=c("CSCI","ASCI_D","ASCI_H"),
                          Ref30=c(0.92, asci_d_thresh[1], asci_h_thresh[1]),
                          Ref10=c(0.79, asci_d_thresh[2], asci_h_thresh[2]),
                          Ref01=c(0.63, asci_d_thresh[3], asci_h_thresh[3]),
                          BCG2=c(1.025, 1.31, 1.23),
                          BCG3=c(0.825, 0.95, 0.97),
                          BCG4=c(0.625, 0.54, 0.67),
                          BCG5=c(0.325, 0, 0.30))

# state_summary.table2<-read_csv("Outputs_stratified/state.summary.csv")
####

mod.id.varz<-c("MasterID","MasterDate","MasterDate_rep", "DevSet","SelectedSample", "PSA2","New_Lat","New_Long")

mod.dat.csci <- mydf.c %>%
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz)) %>%
  rename(IndexScore=CSCI) %>%
  mutate(Index="CSCI",
         # RefThresh = case_when(IndexScore>=thresholds.df$Ref30[which(thresholds.df$Index=="CSCI")]~"Ref30+",
         #                       IndexScore>=thresholds.df$Ref10[which(thresholds.df$Index=="CSCI")]~"Ref10+",
         #                       IndexScore>=thresholds.df$Ref01[which(thresholds.df$Index=="CSCI")]~"Ref01+",
         #                       IndexScore<thresholds.df$Ref01[which(thresholds.df$Index=="CSCI")]~"Ref01-",
         #                       T~"Error"),
         # BCGThresh = case_when(IndexScore>=thresholds.df$BCG2[which(thresholds.df$Index=="CSCI")]~"BCG2",
         #                       IndexScore>=thresholds.df$BCG3[which(thresholds.df$Index=="CSCI")]~"BCG3",
         #                       IndexScore>=thresholds.df$BCG4[which(thresholds.df$Index=="CSCI")]~"BCG4",
         #                       IndexScore>=thresholds.df$BCG5[which(thresholds.df$Index=="CSCI")]~"BCG5",
         #                       IndexScore<thresholds.df$BCG5[which(thresholds.df$Index=="CSCI")]~"BCG6",
         #                       T~"Error"),
         # StateDistThresh = case_when(IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Median")]~"Dist50+",
         #                             IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
         #                             IndexScore<state_summary.table2$California[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
         #                             T~"Error")
  )

# mod.dat.csci$RegDistThresh<-sapply(1:nrow(mod.dat.csci), function(i){
#   psa.i=mod.dat.csci$PSA2[i]
#   IndexScore.i=mod.dat.csci$IndexScore[i]
#   if(psa.i=="Wet")
#     case_when(IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Wet[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
#   else
#     case_when(IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Arid[which(state_summary.table2$variable=="CSCI" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
# })

mod.dat.asci_d<- asci.df.c %>%
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz)) %>%
  rename(IndexScore=ASCI_D) %>%
  mutate(Index="ASCI_D",
         # RefThresh = case_when(IndexScore>=thresholds.df$Ref30[which(thresholds.df$Index=="ASCI_D")]~"Ref30+",
         #                       IndexScore>=thresholds.df$Ref10[which(thresholds.df$Index=="ASCI_D")]~"Ref10+",
         #                       IndexScore>=thresholds.df$Ref01[which(thresholds.df$Index=="ASCI_D")]~"Ref01+",
         #                       IndexScore<thresholds.df$Ref01[which(thresholds.df$Index=="ASCI_D")]~"Ref01-",
         #                       T~"Error"),
         # BCGThresh = case_when(IndexScore>=thresholds.df$BCG2[which(thresholds.df$Index=="ASCI_D")]~"BCG2",
         #                       IndexScore>=thresholds.df$BCG3[which(thresholds.df$Index=="ASCI_D")]~"BCG3",
         #                       IndexScore>=thresholds.df$BCG4[which(thresholds.df$Index=="ASCI_D")]~"BCG4",
         #                       IndexScore>=thresholds.df$BCG5[which(thresholds.df$Index=="ASCI_D")]~"BCG5",
         #                       IndexScore<thresholds.df$BCG5[which(thresholds.df$Index=="ASCI_D")]~"BCG6",
         #                       T~"Error"),
         # StateDistThresh = case_when(IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Median")]~"Dist50+",
         #                             IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
         #                             IndexScore<state_summary.table2$California[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
         #                             T~"Error")
         
  )
# mod.dat.asci_d$RegDistThresh<-sapply(1:nrow(mod.dat.asci_d), function(i){
#   psa.i=mod.dat.asci_d$PSA2[i]
#   IndexScore.i=mod.dat.asci_d$IndexScore[i]
#   if(psa.i=="Wet")
#     case_when(IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
#   else
#     case_when(IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_D" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
# })


mod.dat.asci_h<-asci.df.c %>%
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz)) %>%
  rename(IndexScore=ASCI_H) %>%
  mutate(Index="ASCI_H",
         # RefThresh = case_when(IndexScore>=thresholds.df$Ref30[which(thresholds.df$Index=="ASCI_H")]~"Ref30+",
         #                       IndexScore>=thresholds.df$Ref10[which(thresholds.df$Index=="ASCI_H")]~"Ref10+",
         #                       IndexScore>=thresholds.df$Ref01[which(thresholds.df$Index=="ASCI_H")]~"Ref01+",
         #                       IndexScore<thresholds.df$Ref01[which(thresholds.df$Index=="ASCI_H")]~"Ref01-",
         #                       T~"Error"),
         # BCGThresh = case_when(IndexScore>=thresholds.df$BCG2[which(thresholds.df$Index=="ASCI_H")]~"BCG2",
         #                       IndexScore>=thresholds.df$BCG3[which(thresholds.df$Index=="ASCI_H")]~"BCG3",
         #                       IndexScore>=thresholds.df$BCG4[which(thresholds.df$Index=="ASCI_H")]~"BCG4",
         #                       IndexScore>=thresholds.df$BCG5[which(thresholds.df$Index=="ASCI_H")]~"BCG5",
         #                       IndexScore<thresholds.df$BCG5[which(thresholds.df$Index=="ASCI_H")]~"BCG6",
         #                       T~"Error"),
         # StateDistThresh = case_when(IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Median")]~"Dist50+",
         #                             IndexScore>=state_summary.table2$California[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
         #                             IndexScore<state_summary.table2$California[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
         #                             T~"Error")
         
  )
# mod.dat.asci_h$RegDistThresh<-sapply(1:nrow(mod.dat.asci_h), function(i){
#   psa.i=mod.dat.asci_h$PSA2[i]
#   IndexScore.i=mod.dat.asci_h$IndexScore[i]
#   if(psa.i=="Wet")
#     case_when(IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Wet[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
#   else
#     case_when(IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Median")]~"Dist50+",
#               IndexScore.i>=state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25+",
#               IndexScore.i<state_summary.table2$Arid[which(state_summary.table2$variable=="ASCI_H" & state_summary.table2$Statistic=="Q25")]~"Dist25-",
#               T~"Error")
# })


mod.dat<-bind_rows(mod.dat.csci, mod.dat.asci_d, mod.dat.asci_h)
# mod.dat$RefThresh.f<-  factor(mod.dat$RefThresh, levels=c("Ref30+","Ref10+","Ref01+","Ref01-"))
# mod.dat$BCGThresh.f<-  factor(mod.dat$BCGThresh, levels=c("BCG2","BCG3","BCG4","BCG5","BCG6"))
# mod.dat$StateDistThresh.f<-  factor(mod.dat$StateDistThresh, levels=c("Dist50+","Dist25+","Dist25-"))
# mod.dat$RegDistThresh.f<-  factor(mod.dat$RegDistThresh, levels=c("Dist50+","Dist25+","Dist25-"))

# mod.dat %>%
#   group_by(Index, BCGThresh.f) %>%
#   tally() %>%
#   pivot_wider(names_from=c(Index),values_from = n, values_fill = 0)

# mod.dat %>%
#   group_by(Index, RegDistThresh.f) %>%
#   tally() %>%
#   pivot_wider(names_from=c(Index),values_from = n,values_fill = 0)


# mod.dat$Ref30<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$Ref30[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$Ref30<-factor(mod.dat$Ref30, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$Ref10<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$Ref10[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$Ref10<-factor(mod.dat$Ref10, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$Ref01<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$Ref01[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$Ref01<-factor(mod.dat$Ref01, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$BCG2<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$BCG2[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$BCG2<-factor(mod.dat$BCG2, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$BCG3<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$BCG3[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$BCG3<-factor(mod.dat$BCG3, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$BCG4<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$BCG4[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$BCG4<-factor(mod.dat$BCG4, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$BCG5<-sapply(1:nrow(mod.dat), function(i){
#   ind.i<-mod.dat$Index[i]
#   thresh.i<-thresholds.df$BCG5[which(thresholds.df$Index==ind.i)]
#   ifelse(mod.dat$IndexScore[i]<thresh.i,"DoesNotMeet","MeetsObjective")})
# mod.dat$BCG5<-factor(mod.dat$BCG5, levels=c("DoesNotMeet","MeetsObjective"))
# 
# mod.dat$StateDist50<-ifelse(mod.dat$StateDistThresh %in% c("Dist50+"),"MeetsObjective","DoesNotMeet")
# mod.dat$StateDist50<-factor(mod.dat$StateDist50, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$StateDist25<-ifelse(mod.dat$StateDistThresh %in% c("Dist50+","Dist25+"),"MeetsObjective","DoesNotMeet")
# mod.dat$StateDist25<-factor(mod.dat$StateDist25, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$RegDist50<-ifelse(mod.dat$RegDistThresh %in% c("Dist50+"),"MeetsObjective","DoesNotMeet")
# mod.dat$RegDist25<-ifelse(mod.dat$RegDistThresh %in% c("Dist50+","Dist25+"),"MeetsObjective","DoesNotMeet")
# mod.dat$RegDist50<-factor(mod.dat$RegDist50, levels=c("DoesNotMeet","MeetsObjective"))
# mod.dat$RegDist25<-factor(mod.dat$RegDist25, levels=c("DoesNotMeet","MeetsObjective"))


#Model summaries
model.summary<-crossing(Response=response.varz,
                        BiostimVar=c(chem.varz, om.varz),
                        # BIgoal=c("Ref30","Ref10","Ref01",
                        #          "BCG3","BCG4",
                        #          "StateDist50","StateDist25",
                        #          "RegDist50","RegDist25"),
                        Stratum=c("California"))# %>%
# filter(!(Stratum=="California" & BIgoal %in% c("RegDist50","RegDist25"))) %>%
# filter(!(Stratum!="California" & BIgoal %in% c("StateDist50","StateDist25")))
model.summary<-inner_join(bs.varz.df, model.summary) #%>%
# mutate(BIgoal2 = case_when(BIgoal %in% c("StateDist50","RegDist50")~"Dist50",
# BIgoal %in% c("StateDist25","RegDist25")~"Dist25",
# T~BIgoal))

# table(mod.dat$Index, mod.dat$RefThresh.f)
# table(mod.dat$Index, mod.dat$RegDistThresh.f)

#######
#CREATE MODELS
#######
library(scam)
my.models<-
  lapply(1:nrow(model.summary), function(i){
    ind.i<-model.summary$Response[i]
    stim.i<-model.summary$BiostimVar[i]
    # goal.i<-model.summary$BIgoal[i]
    # strat.i<-model.summary$Stratum[i]
    print(paste(i, ind.i, stim.i))
    my.dat<-mod.dat[which(mod.dat$DevSet=="Cal" & mod.dat$SelectedSample=="Selected" & mod.dat$Index==ind.i),
                    c(mod.id.varz,stim.i, "IndexScore")] %>% na.omit()
    # if(strat.i=="California")
    # my.dat<-my.dat
    names(my.dat)<-c(mod.id.varz,"Biostim","IndexScore")
    my.dat<-droplevels(my.dat)
    scam(IndexScore~s(Biostim, bs="mpd"), data=my.dat)
  })

my.models_null<-
  lapply(1:nrow(model.summary), function(i){
    ind.i<-model.summary$Response[i]
    stim.i<-model.summary$BiostimVar[i]
    # goal.i<-model.summary$BIgoal[i]
    # strat.i<-model.summary$Stratum[i]
    print(paste(i, ind.i, stim.i))
    my.dat<-mod.dat[which(mod.dat$DevSet=="Cal" & mod.dat$SelectedSample=="Selected" & mod.dat$Index==ind.i),
                    c(mod.id.varz,stim.i, "IndexScore")] %>% na.omit()
    # if(strat.i=="California")
    # my.dat<-my.dat
    names(my.dat)<-c(mod.id.varz,"Biostim","IndexScore")
    my.dat<-droplevels(my.dat)
    scam(IndexScore~1, data=my.dat)
  })


model.summary$AIC<-sapply(my.models, function(x) x$aic)
model.summary$AIC_null<-sapply(my.models_null, function(x) x$aic)
model.summary$deviance_explained<-sapply(my.models, function(x) x$deviance)
model.summary$gcv.ubre<-sapply(my.models, function(x) x$gcv.ubre)
model.summary$dgcv.ubre<-sapply(my.models, function(x) x$dgcv.ubre)

length.out.x<-1000
my.newdfs<-data.frame(Nitrogen_Total_mgPerL=seq(from=0, to=3, length.out=length.out.x),
                      Phosphorus_as_P_mgPerL=seq(from=0, to=1.5, length.out=length.out.x),
                      Chlorophyll_a_mgPerm2=seq(from=0, to=300, length.out=length.out.x),
                      Ash_Free_Dry_Mass_mgPercm2=seq(from=0, to=40, length.out=length.out.x),
                      PCT_MAP=seq(from=0, to=100, length.out=length.out.x))

junk.pred = data.frame(Biostim=my.newdfs[,1])
# names(junk.pred)="Biostim"
predict(my.models[[1]], newdata=junk.pred, type="response", se=T)

my.models.predictions<-lapply(1:nrow(model.summary), function(i){
  bs.i=model.summary$BiostimVar[i]
  ind.i=model.summary$Response[i]
  xdf = data.frame(Biostim=my.newdfs[,bs.i])
  mod.i=my.models[[i]]
  ydf=predict(mod.i, newdata=xdf, type="response", se=T)
  zdf=tibble(BiostimVar=bs.i, 
             BiostimValue = xdf$Biostim, 
             Index=ind.i,
             Fit=ydf$fit, SE=ydf$se.fit)
  zdf
}) %>% bind_rows() %>%
  inner_join(bs.varz.df)
my.models.predictions$BSPretty<-factor(my.models.predictions$BSPretty, levels=bs.varz.df$BSPretty)

obs_points_df <- mod.dat %>%
  pivot_longer(cols=all_of(bs.varz), names_to="BiostimVar", values_to = "BiostimValue", values_drop_na = T) %>%
  filter(!(BiostimVar=="Nitrogen_Total_mgPerL" & BiostimValue>3)) %>%
  filter(!(BiostimVar=="Phosphorus_as_P_mgPerL" & BiostimValue>1.5)) %>%
  filter(!(BiostimVar=="Chlorophyll_a_mgPerm2" & BiostimValue>300)) %>%
  filter(!(BiostimVar=="Ash_Free_Dry_Mass_mgPercm2" & BiostimValue>40))  %>%
  inner_join(bs.varz.df)
obs_points_df$BSPretty<-factor(obs_points_df$BSPretty, levels=bs.varz.df$BSPretty)
scam_plots<-ggplot(data=my.models.predictions, aes(x=BiostimValue, y=Fit))+
  geom_point(data=obs_points_df, size=.5, aes(y=IndexScore))+
  geom_ribbon(aes(ymin=Fit-1.96*SE, ymax=Fit+1.96*SE), alpha=.2, fill="#39568cff")+
  geom_path( linewidth=1, color="#39568cff")+
  facet_grid(Index~BSPretty, scales="free_x")+
  xlab("")+ylab("Index score")+
  theme_bw()
ggsave("Figures/scam_plots.jpg", height=6, width=10)


FindThresholds <-function(goal, 
                          index=c("CSCI","ASCI_D","ASCI_H"), 
                          stressor=c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP")
) {
  if(length(goal)>1) {
    stop(stop("Incorrect number of goals entered.\nOnly a single goal may be assessed at this time"))
  }
  # if (length(goal)>1 & length(goal)!=length(index)) {
  #       stop("Incorrect number of goals entered.\nEnter a single goal for all indices, or enter one goal for each index")
  # }
  
  xdf = my.models.predictions %>%
    filter(BiostimVar %in% stressor) %>%
    filter(Index %in% index) %>%
    filter(Fit>=goal) %>% #Need to update for different goals for each index?
    group_by(BiostimVar,Index) %>%
    slice_max(BiostimValue, n=1) %>%
    ungroup() %>%
    rename(Stressor=BiostimVar, 
           Threshold_candidate=BiostimValue, 
           IndexScore_predicted=Fit,
           IndexScore_predicted_se=SE) %>%
    mutate(IndexScore_predicted_l95=IndexScore_predicted-IndexScore_predicted_se*1.96,
           IndexScore_predicted_u95=IndexScore_predicted+IndexScore_predicted_se*1.96,)
  xdf
}
FindThresholds(goal=0.75, index=c("CSCI"), stressor="PCT_MAP")
FindThresholds(goal=0.75)
FindThresholds(goal=c(.65,.6))

FindThresholds_plot<-function(goal, 
                              index=c("CSCI","ASCI_D","ASCI_H"), 
                              stressor=c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP")
) {
  xdf= FindThresholds(goal=goal, index=index, stressor=stressor) %>%
    mutate(Threshold_candidate_rounded = case_when(Stressor %in% c("Nitrogen_Total_mgPerL","Phosphorus_as_P_mgPerL")~round(Threshold_candidate,3),
                                                   T~round(Threshold_candidate,0)))
  
  
  ggplot(data=my.models.predictions %>%
           filter(BiostimVar %in% stressor) %>%
           filter(Index %in% index), aes(x=BiostimValue, y=Fit))+
    geom_point(data=obs_points_df %>%
                 filter(BiostimVar %in% stressor) %>%
                 filter(Index %in% index),
               size=.5, aes(y=IndexScore))+
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

FindThresholds_plot(goal=c(.7))
#####
######

WadeableStreams_thresholds<-
  tibble(Class="Wadeable streams",
         ThresholdType="Reference",
         Stringency = c("High","Intermediate","Low"),
         CSCI=c(.92,.79,.63),
         ASCI_D=c(.94,.86,.75),
         ASCI_H=c(.94,.86,.75)
  )

mazor2022_thresholds <-read_csv("Outputs_stratified/model.summary_thresholds_woInorganics.csv") %>%
  filter(BIgoal %in% c("Ref01","Ref10","Ref30"),
         Stratum == "California") %>%
  transmute(Class="Wadeable streams",
            ThresholdType=paste0("Response_LR-",Response),
            BSPretty,
            Stringency = case_when(BIgoal=="Ref30"~"High",
                                   BIgoal=="Ref10"~"Intermediate",
                                   BIgoal=="Ref01"~"Low",
                                   T~"x"),
            Threshold=p80) %>%
  pivot_wider(names_from=BSPretty, values_from = Threshold)
write.table(mazor2022_thresholds, file="clipboard",sep="\t", row.names=F)

bi_thresholds_df<-structure(list(Threshold.type = c("Reference", "Reference", "Reference", 
                                                    "Reference", "Reference", "Reference", "Reference", "Reference", 
                                                    "Reference", "Best observed", "Best observed", "Best observed", 
                                                    "Best observed", "Best observed", "Best observed", "Best observed", 
                                                    "Best observed", "Best observed", "Best observed", "Best observed", 
                                                    "Best observed", "Best observed", "Best observed", "Best observed", 
                                                    "Best observed"), 
                                 Population = c("Wadeable (standard)", "Wadeable (standard)", 
                                                "Wadeable (standard)", "RFI-N", "RFI-N", "RFI-N", "RFI-S", "RFI-S", 
                                                "RFI-S", "CVF", "CVF", "CVF", "SB0", "SB0", "SB0", "SB1", "SB1", 
                                                "SB1", "SB2", "SB2", "SB2", "HB", "HB", "HB", "CC"), 
                                 Index = c("ASCI_D", 
                                           "ASCI_H", "CSCI", "ASCI_D", "ASCI_H", "CSCI", "ASCI_D", "ASCI_H", 
                                           "CSCI", "ASCI_D", "ASCI_H", "CSCI", "ASCI_D", "ASCI_H", "CSCI", 
                                           "ASCI_D", "ASCI_H", "CSCI", "ASCI_D", "ASCI_H", "CSCI", "ASCI_D", 
                                           "ASCI_H", "CSCI", "CSCI"), 
                                 Standard.usage.supported. = c("Yes", 
                                                               "Yes", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", 
                                                               "Yes", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "No", "No", 
                                                               "Yes", "Yes", "Yes", "No", "No"), 
                                 n = c(418L, 418L, 473L, 32L, 
                                       32L, 79L, 45L, 45L, 53L, 89L, 86L, 301L, 51L, 51L, 78L, 36L, 
                                       36L, 52L, 57L, 57L, 67L, 152L, 152L, 203L, 65L), 
                                 High.stringency = c(0.95, 
                                                     0.95, 0.92, 0.97, 0.95, 0.69, 0.94, 0.92, 0.89, 1.13, 1.05, 0.85, 
                                                     1.01, 0.94, 0.99, 1.01, 0.97, 1.1, 0.93, 0.88, 0.96, 1.05, 1.02, 
                                                     0.74, 0.53),
                                 Intermediate = c(0.86, 0.86, 0.79, 0.91, 0.89, 0.54, 
                                                  0.81, 0.85, 0.85, 1.02, 0.94, 0.67, 0.77, 0.79, 0.78, 0.85, 0.86, 
                                                  1, 0.77, 0.76, 0.75, 0.88, 0.87, 0.67, 0.45), 
                                 Low.stringency = c(0.75, 
                                                    0.75, 0.63, 0.8, 0.75, 0.46, 0.61, 0.59, 0.71, 0.92, 0.8, 0.52, 
                                                    0.68, 0.64, 0.66, 0.68, 0.67, 0.81, 0.64, 0.6, 0.64, 0.74, 0.74, 
                                                    0.55, 0.37)), 
                            class = "data.frame", 
                            row.names = c(NA, -25L))

bi_thresholds_df_scam<-bi_thresholds_df %>% 
  select(Index, High.stringency, Intermediate, Low.stringency) %>%
  pivot_longer(cols=c(High.stringency, Intermediate, Low.stringency)) %>%
  unique() %>%
  crossing(BiostimVar = c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", "Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP"))

bi_thresholds_df_scam$SCAM_threshold<-sapply(1:nrow(bi_thresholds_df_scam), function(i){
  ind.i=bi_thresholds_df_scam$Index[i]
  bs.i= bi_thresholds_df_scam$BiostimVar[i]
  goal.i = bi_thresholds_df_scam$value[i]
  xdf=FindThresholds(goal=goal.i, index=ind.i, stressor=bs.i)
  xdf$Threshold_candidate[1]
})
bi_thresholds_df_scam$SCAM_threshold

scam_thresholds<-bi_thresholds_df %>%
  pivot_longer(cols=c(High.stringency, Intermediate, Low.stringency))  %>%
  left_join(
    bi_thresholds_df_scam 
  ) %>%
  pivot_wider(names_from=BiostimVar, values_from=SCAM_threshold) %>%
  arrange(Population, Index, name) %>%
  transmute(Class=case_when(Population=="Wadeable (standard)"~"Wadeable streams",T~Population),
            Threshold.type=paste0("Response_SCAM-",Index),
            Stringency=name %>% str_replace(".stringency",""),
            TN = Nitrogen_Total_mgPerL, TP = Phosphorus_as_P_mgPerL,
            Chla=Chlorophyll_a_mgPerm2, AFDM=Ash_Free_Dry_Mass_mgPercm2, PCT_MAP)
write.table(scam_thresholds, file="clipboard", sep="\t", row.names=F)


bi_thresholds_df %>%
  rename(High=High.stringency, Low=Low.stringency) %>% 
  select(-Standard.usage.supported., -n) %>%
  pivot_longer(cols=c(High, Intermediate, Low), names_to = "Stringency") %>%
  pivot_wider(names_from=Index, values_from = value) %>%
  transmute(Class=case_when(Population=="Wadeable (standard)"~"Wadeable streams",T~Population),
            ThresholdType=Threshold.type, 
            Stringency,
            CSCI, ASCI_D, ASCI_H) %>%
  write.table( file="clipboard", sep="\t", row.names=F)
  

FindThresholds(goal=0.7)
FindThresholds_plot(goal=.7)

my.models.predictions %>%
  filter(Index=="ASCI_D" & BiostimVar=="PCT_MAP")
############################

# Three things are needed for a shiny app
# my.models.predictions 
# FindThresholds
# FindThresholds_plot

# User specifies:
# a goal (required)
# an index or vector of indices (defaults to all 3)
# and a stressor or vector of stressors (defaults to all 5)

write.table(my.models.predictions, file="clipboard-20000", sep="\t", row.names=F)
write_csv(my.models.predictions %>% as.data.frame(),
          file="continuous_model_data/my.models.predictions.csv")
write_csv(obs_points_df %>% as.data.frame(),
          file="continuous_model_data/obs_points_df.csv")
save(my.models, file="continuous_model_data/my.models.Rdata")

