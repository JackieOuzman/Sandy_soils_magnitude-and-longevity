### This is the data and working for Handndorf meeting 2/3/2023 

#run the R marksdown first to update / refresh the dataset
library(ggplot2)
library(readxl)
library(tidyverse)
#library(multcompView)

library(dplyr)
library(tidyverse)

library(stringr)

Handndorf <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/Jackie/summary_control_data_all2023-03-02.csv")
str(Handndorf)
Handndorf <- Handndorf %>% dplyr::filter(is.na(amendment_code1))

################################################################################
### keep sites with Moderate / Severe soil strength

Handndorf$Physical <- factor(
  Handndorf$Physical,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)
unique(Handndorf$Physical)
str(Handndorf)
unique(Handndorf$soil_modification)



###############################################################################
## keep sites with ripping with no amendments

Handndorf_rip_no_amend <- Handndorf %>% 
  dplyr::filter(is.na(amendment_code1)) %>% 
  dplyr::filter(soil_modification == "Rip")
###############################################################################

Handndorf_rip_no_amend_box_plot <- Handndorf_rip_no_amend %>%  
    filter(!is.na(yr_post_amelioration )) %>% 
    filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

Handndorf_rip_no_amend_box_plot




summary_Handndorf_rip_no_amend <-
  Handndorf_rip_no_amend %>%
  dplyr::group_by(Physical,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_no_amend


###############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_rip_no_amend_ave_yield <- Handndorf_rip_no_amend %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_rip_no_amend_cum_yield <- soil_strength_rip_no_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_rip_no_amend_cum_yield


soil_strength_rip_no_amend_cum_yield_plot <- soil_strength_rip_no_amend_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_rip_no_amend_cum_yield_plot

soil_strength_rip_no_amend_cum_yield_plot <- soil_strength_rip_no_amend_cum_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "Rip")
soil_strength_rip_no_amend_cum_yield_plot <- soil_strength_rip_no_amend_cum_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_rip_no_amend_cum_yield_plot
soil_strength_rip_no_amend_cum_yield_plot <- soil_strength_rip_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_rip_no_amend_cum_yield_plot
cum_SS_rip <- soil_strength_rip_no_amend_cum_yield_plot
###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


soil_strength_rip_no_amend_cum_yield_plot %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  #geom_point(aes(color=treatment))+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "Constraint = Repellence",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)



###############################################################################
soil_strength_rip_no_amend_cum_yield_plot
###############################################################################



###############################################################################
## keep sites with ripping with no amendments and suss out ripping depth

#From above Handndorf_rip_no_amend
Handndorf_rip_no_amend <- Handndorf %>% 
  dplyr::filter(is.na(amendment_code1)) %>% 
  dplyr::filter(soil_modification == "Rip")

str(Handndorf_rip_no_amend)
unique(Handndorf_rip_no_amend$Rip_depth_jax)

#Handndorf_rip_no_amend$Rip_depth_jax <- as.double(Handndorf_rip_no_amend$Rip_depth_jax)

Handndorf_rip_no_amend<- Handndorf_rip_no_amend %>% 
  mutate(rip_depth_code = case_when(
    Rip_depth_jax == "30" ~ "<=40",
    Rip_depth_jax == "35" ~ "<=40",
    Rip_depth_jax == "40" ~ "<=40",
    TRUE ~ ">40"
    
  ))

unique(Handndorf_rip_no_amend$rip_depth_code)
###############################################################################


Handndorf_rip_depth_no_amend_box_plot <- Handndorf_rip_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  filter(rip_depth_code == "<=40") %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

Handndorf_rip_depth_no_amend_box_plot


str(Handndorf_soil_strength)

summary_Handndorf_rip_dpeth_no_amend <-
  Handndorf_rip_no_amend %>% 
  filter(rip_depth_code == "<=40") %>%
  dplyr::group_by(Physical,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_dpeth_no_amend

summary_Handndorf_rip_dpeth_no_amend <- summary_Handndorf_rip_dpeth_no_amend %>% 
  arrange(Physical, yr_post_amelioration)
summary_Handndorf_rip_dpeth_no_amend
summary_Handndorf_rip_dpeth_no_amend <- summary_Handndorf_rip_dpeth_no_amend %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(n >1)

summary_Handndorf_rip_dpeth_no_amend



###############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_rip_depth_no_amend_ave_yield <- Handndorf_rip_no_amend %>% 
  filter(rip_depth_code == "<=40") %>%
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_rip_depth_no_amend_cum_yield <- soil_strength_rip_depth_no_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_rip_depth_no_amend_cum_yield


soil_strength_rip_depth_no_amend_cum_yield_plot <- soil_strength_rip_depth_no_amend_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_rip_depth_no_amend_cum_yield_plot

soil_strength_rip_depth_no_amend_cum_yield_plot <- soil_strength_rip_depth_no_amend_cum_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "Rip <=40")
soil_strength_rip_depth_no_amend_cum_yield_plot <- soil_strength_rip_depth_no_amend_cum_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_rip_depth_no_amend_cum_yield_plot
soil_strength_rip_depth_no_amend_cum_yield_plot <- soil_strength_rip_depth_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_rip_depth_no_amend_cum_yield_plot

cum_SS_rip_less40 <- soil_strength_rip_depth_no_amend_cum_yield_plot
###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge

cum_SS_rip
cum_SS_rip_less40

cum_SS_options <- rbind(cum_SS_rip, cum_SS_rip_less40)

cum_SS_options


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_SS_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)





###############################################################################


Handndorf_rip_depth_no_amend_box_plot <- Handndorf_rip_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  filter(rip_depth_code == ">40") %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

Handndorf_rip_depth_no_amend_box_plot


str(Handndorf_soil_strength)

summary_Handndorf_rip_dpeth2_no_amend <-
  Handndorf_rip_no_amend %>% 
  filter(rip_depth_code == ">40") %>%
  dplyr::group_by(Physical,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_dpeth2_no_amend

summary_Handndorf_rip_dpeth2_no_amend <- summary_Handndorf_rip_dpeth2_no_amend %>% 
  arrange(Physical, yr_post_amelioration)

summary_Handndorf_rip_dpeth2_no_amend <- summary_Handndorf_rip_dpeth2_no_amend %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(n >1)

summary_Handndorf_rip_dpeth2_no_amend



###############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_rip_depth2_no_amend_ave_yield <- Handndorf_rip_no_amend %>% 
  filter(rip_depth_code == ">40") %>%
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_rip_depth2_no_amend_cum_yield <- soil_strength_rip_depth2_no_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_rip_depth2_no_amend_cum_yield


soil_strength_rip_depth2_no_amend_cum_yield_plot <- soil_strength_rip_depth2_no_amend_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_rip_depth2_no_amend_cum_yield_plot

soil_strength_rip_depth2_no_amend_cum_yield_plot <- soil_strength_rip_depth2_no_amend_cum_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "Rip >40")
soil_strength_rip_depth2_no_amend_cum_yield_plot <- soil_strength_rip_depth2_no_amend_cum_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_rip_depth2_no_amend_cum_yield_plot
soil_strength_rip_depth2_no_amend_cum_yield_plot <- soil_strength_rip_depth2_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_rip_depth2_no_amend_cum_yield_plot

cum_SS_rip_great40 <- soil_strength_rip_depth2_no_amend_cum_yield_plot
###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge

cum_SS_rip
cum_SS_rip_less40
cum_SS_rip_great40

cum_SS_options <- rbind(cum_SS_rip, cum_SS_rip_less40, cum_SS_rip_great40)

cum_SS_options


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_SS_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)









###############################################################################
## keep sites with  spading with no amendments

Handndorf_soil_strength_spade_no_amend <- Handndorf %>% 
  dplyr::filter(is.na(amendment_code1)) %>% 
  dplyr::filter(soil_modification == "Spade")
###############################################################################

soil_strength_spade_no_Amend_box_plot <- Handndorf_soil_strength_spade_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

soil_strength_spade_no_Amend_box_plot



summary_soil_strength_spade_no_Amend <-
  Handndorf_soil_strength_spade_no_amend %>%
  dplyr::group_by(Physical,
    yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n = n())


summary_soil_strength_spade_no_Amend

summary_soil_strength_spade_no_Amend <- summary_soil_strength_spade_no_Amend %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(n >1)
summary_soil_strength_spade_no_Amend <- summary_soil_strength_spade_no_Amend %>% 
  arrange(Physical, yr_post_amelioration)

summary_soil_strength_spade_no_Amend




###############################################################################
## cum yield for soil strength spade no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_spade_no_amend_ave_yield <- Handndorf_soil_strength_spade_no_amend %>% 
   group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_spade_no_amend_cum_yield <- soil_strength_spade_no_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_spade_no_amend_cum_yield

View(soil_strength_spade_no_amend_cum_yield)
soil_strength_spade_no_amend_cum_yield_plot <- soil_strength_spade_no_amend_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_spade_no_amend_cum_yield_plot

soil_strength_spade_no_amend_cum_yield_plot <- soil_strength_spade_no_amend_cum_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "Spade")
soil_strength_spade_no_amend_cum_yield_plot <- soil_strength_spade_no_amend_cum_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_spade_no_amend_cum_yield_plot
soil_strength_spade_no_amend_cum_yield_plot <- soil_strength_spade_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_spade_no_amend_cum_yield_plot

cum_SS_spade <- soil_strength_spade_no_amend_cum_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge

cum_SS_rip
cum_SS_rip_less40
cum_SS_rip_great40
cum_SS_spade

cum_SS_options <- rbind(cum_SS_rip, cum_SS_rip_less40, cum_SS_rip_great40, cum_SS_spade)

cum_SS_options


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_SS_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,4))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)





###############################################################################
## keep sites with  spading with no amendments
unique(Handndorf$soil_modification)
Handndorf_soil_strength_IncRip_no_amend
Handndorf_soil_strength_IncRip_no_amend <- Handndorf %>% 
  dplyr::filter(is.na(amendment_code1)) %>% 
  dplyr::filter(soil_modification == "IncRip")

unique(Handndorf_soil_strength_IncRip_no_amend$soil_modification)
###############################################################################

Handndorf_soil_strength_IncRip_no_amend_box_plot <- Handndorf_soil_strength_IncRip_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

Handndorf_soil_strength_IncRip_no_amend_box_plot



summary_Handndorf_soil_strength_IncRip_no_amend <-
  Handndorf_soil_strength_IncRip_no_amend %>%
  dplyr::group_by(Physical,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n = n())


summary_Handndorf_soil_strength_IncRip_no_amend

summary_Handndorf_soil_strength_IncRip_no_amend <- summary_Handndorf_soil_strength_IncRip_no_amend %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(n >1)
summary_Handndorf_soil_strength_IncRip_no_amend <- summary_Handndorf_soil_strength_IncRip_no_amend %>% 
  arrange(Physical, yr_post_amelioration)

summary_Handndorf_soil_strength_IncRip_no_amend




###############################################################################
## cum yield for soil strength spade no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_InRip_no_amend_ave_yield <- Handndorf_soil_strength_IncRip_no_amend %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_InRip_no_amend_ave_yield <- soil_strength_InRip_no_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_InRip_no_amend_ave_yield

View(soil_strength_InRip_no_amend_ave_yield)
soil_strength_InRip_no_amend_ave_yield_plot <- soil_strength_InRip_no_amend_ave_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_InRip_no_amend_ave_yield_plot

soil_strength_InRip_no_amend_ave_yield_plot <- soil_strength_InRip_no_amend_ave_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "InRip")
soil_strength_InRip_no_amend_ave_yield_plot <- soil_strength_InRip_no_amend_ave_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_spade_no_amend_cum_yield_plot
soil_strength_spade_no_amend_cum_yield_plot <- soil_strength_spade_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_InRip_no_amend_ave_yield_plot

cum_SS_InRip <- soil_strength_InRip_no_amend_ave_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge

cum_SS_rip
cum_SS_rip_less40
cum_SS_rip_great40
cum_SS_spade
cum_SS_InRip

cum_SS_options <- rbind(cum_SS_rip, cum_SS_rip_less40, cum_SS_rip_great40, cum_SS_spade, cum_SS_InRip)

cum_SS_options


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_SS_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)

############################################################################






































#############################################################################
############         Repellence         ######################################
#############################################################################
## ###############################################################################
### 
str(Handndorf)

Handndorf_rip_no_amend$Repellence <- factor(
  Handndorf_rip_no_amend$Repellence,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)
unique(Handndorf$Repellence)
str(Handndorf)
unique(Handndorf$soil_modification)



###############################################################################

Handndorf_rip_no_amend_box_plot <- Handndorf_rip_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Repellence)

Handndorf_rip_no_amend_box_plot




summary_Handndorf_rip_no_amend <-
  Handndorf_rip_no_amend %>%
  dplyr::group_by(Repellence,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_no_amend






##############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
repl_rip_no_amend_ave_yield <- Handndorf_rip_no_amend %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
repl_rip_no_amend_cum_yield <- repl_rip_no_amend_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
repl_rip_no_amend_cum_yield

View(repl_rip_no_amend_cum_yield)
repl_rip_no_amend_cum_yield_cum_yield_plot <- repl_rip_no_amend_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
repl_rip_no_amend_cum_yield_cum_yield_plot

repl_rip_no_amend_cum_yield_plot <- repl_rip_no_amend_cum_yield_cum_yield_plot %>% 
  mutate(constraint = "Repellency",
         treatment = "Rip")
repl_rip_no_amend_cum_yield_plot <- repl_rip_no_amend_cum_yield_plot %>% 
  arrange(Repellence, yr_post_amelioration)
repl_rip_no_amend_cum_yield_plot
repl_rip_no_amend_cum_yield_plot <- repl_rip_no_amend_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

repl_rip_no_amend_cum_yield_plot

cum_Repl_rip <- repl_rip_no_amend_cum_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge


cum_Repl_options <- cum_Repl_rip


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_Repl_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Repellence     )










###############################################################################
## Repellence and Inc rip

str(Handndorf)
unique(Handndorf$soil_modification)

Handndorf_IncRip <- Handndorf %>%  filter(soil_modification == "IncRip")
Handndorf_IncRip$Repellence <- factor(
  Handndorf_IncRip$Repellence,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)

Handndorf_IncRip_no_amd_amend_box_plot <- Handndorf_IncRip %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Repellence)

Handndorf_IncRip_no_amd_amend_box_plot




summary_Handndorf_IncRip <-
  Handndorf_IncRip %>%
  dplyr::group_by(Repellence,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_IncRip






##############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
Handndorf_IncRip_ave_yield <- Handndorf_IncRip %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
Handndorf_IncRip_cum_yield <- Handndorf_IncRip_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
Handndorf_IncRip_cum_yield


Handndorf_IncRip_cum_yield_plot <- Handndorf_IncRip_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
Handndorf_IncRip_cum_yield_plot

Handndorf_IncRip_cum_yield_plot <- Handndorf_IncRip_cum_yield_plot %>% 
  mutate(constraint = "Repellency",
         treatment = "IncRip")
Handndorf_IncRip_cum_yield_plot <- Handndorf_IncRip_cum_yield_plot %>% 
  arrange(Repellence, yr_post_amelioration)
Handndorf_IncRip_cum_yield_plot
Handndorf_IncRip_cum_yield_plot <- Handndorf_IncRip_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

Handndorf_IncRip_cum_yield_plot

cum_Repl_Inrip <- Handndorf_IncRip_cum_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge
cum_Repl_rip
cum_Repl_Inrip

cum_Repl_options <- rbind(cum_Repl_rip, cum_Repl_Inrip)


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_Repl_options %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,4))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Repellence     )




###############################################################################
## Repellence and spading

str(Handndorf)
unique(Handndorf$soil_modification)
unique(Handndorf$Repellence)


unique(Handndorf_spading$Repellence)
Handndorf_spading <- Handndorf %>%  filter(soil_modification == "Spade")
unique(Handndorf_spading$Repellence)

Handndorf_spading$Repellence <- factor(
  Handndorf_spading$Repellence,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)



str(Handndorf)
unique(Handndorf_spading$soil_modification)




Handndorf_spading_no_amd_amend_box_plot <- Handndorf_spading %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Repellence)

Handndorf_spading_no_amd_amend_box_plot




summary_Handndorf_spading <-
  Handndorf_spading %>%
  dplyr::group_by(Repellence,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_spading






##############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
Handndorf_spading_ave_yield <- Handndorf_spading %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
Handndorf_IncRip_cum_yield <- Handndorf_spading_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
Handndorf_IncRip_cum_yield


Handndorf_spade_cum_yield_plot <- Handndorf_IncRip_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
Handndorf_spade_cum_yield_plot

Handndorf_spade_cum_yield_plot <- Handndorf_spade_cum_yield_plot %>% 
  mutate(constraint = "Repellency",
         treatment = "Spade")
Handndorf_spade_cum_yield_plot <- Handndorf_spade_cum_yield_plot %>% 
  arrange(Repellence, yr_post_amelioration)
Handndorf_spade_cum_yield_plot
Handndorf_spade_cum_yield_plot <- Handndorf_spade_cum_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

Handndorf_spade_cum_yield_plot

cum_Repl_spade <- Handndorf_spade_cum_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge
cum_Repl_rip
cum_Repl_Inrip
cum_Repl_spade

cum_Repl_options <- rbind(cum_Repl_rip, cum_Repl_Inrip, cum_Repl_spade)


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_Repl_options %>%
  filter(yr_post_amelioration < 4) %>%
  filter(treatment!= "IncRip") %>% 
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Repellence     )



##############################################################################
## Repellence and wetting agents - can I do this? Nope I don't think so
All_Handndorf <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/Jackie/summary_control_data_all2023-03-02.csv")
str(All_Handndorf)
All_Handndorf %>%  distinct(Descriptors ) %>%  arrange(Descriptors)



##############################################################################
## Acidity and ripping -


Handndorf_rip_no_amend
str(Handndorf_rip_no_amend)
unique(Handndorf_rip_no_amend$soil_modification)
unique(Handndorf_rip_no_amend$Acidity )


Handndorf_rip_no_amend$Acidity <- factor(
  Handndorf_rip_no_amend$Acidity,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)


suss_out_acid_mod <- Handndorf_rip_no_amend %>% filter(Acidity == "Moderate issue")


Handndorf_rip_no_amend_box_plot <- Handndorf_rip_no_amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Acidity)

Handndorf_rip_no_amend_box_plot




summary_Handndorf_rip_no_amend <-
  Handndorf_rip_no_amend %>%
  dplyr::group_by(Acidity,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_no_amend






##############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
Handndorf_rip_no_amend_ave_yield <- Handndorf_rip_no_amend %>% 
  group_by(site, yr_post_amelioration, Acidity) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
Handndorf_rip_no_amend_ave_yield <- Handndorf_rip_no_amend_ave_yield %>%
  group_by(site, Acidity) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
Handndorf_rip_no_amend_ave_yield


Handndorf_rip_no_amend_ave_yield_plot <- Handndorf_rip_no_amend_ave_yield %>% 
  group_by(yr_post_amelioration, Acidity) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
Handndorf_rip_no_amend_ave_yield_plot

Handndorf_rip_no_amend_ave_yield_plot <- Handndorf_rip_no_amend_ave_yield_plot %>% 
  mutate(constraint = "Acidity",
         treatment = "Rip")
Handndorf_rip_no_amend_ave_yield_plot <- Handndorf_rip_no_amend_ave_yield_plot %>% 
  arrange(Acidity, yr_post_amelioration)
Handndorf_rip_no_amend_ave_yield_plot
Handndorf_rip_no_amend_ave_yield_plot <- Handndorf_rip_no_amend_ave_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

Handndorf_rip_no_amend_ave_yield_plot

cum_Acid_Rip <- Handndorf_rip_no_amend_ave_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge
cum_Acid_Rip


cum_Repl_options <- cum_Acid_Rip


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_Repl_options %>%
  filter(yr_post_amelioration < 4) %>%
  filter(treatment!= "IncRip") %>% 
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(-2,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Acidity     )



##############################################################################




##############################################################################
## Acidity and ripping - but with amendments

str(All_Handndorf)
unique(All_Handndorf$soil_modification)
Handndorf_rip_Amend <- All_Handndorf %>%  filter(soil_modification == 'Rip')

Handndorf_rip_Amend
str(Handndorf_rip_Amend)
unique(Handndorf_rip_Amend$soil_modification)
unique(Handndorf_rip_Amend$Acidity )


Handndorf_rip_Amend$Acidity <- factor(
  Handndorf_rip_Amend$Acidity,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)



#get lime or clay we only had a few with clay eg Yenda but that was a sweep
#most of the clay was added with something else and is coded mixed
unique(Handndorf_rip_Amend$amendment_code1)
Handndorf_rip_AmendClay <- Handndorf_rip_Amend %>% 
  filter(amendment_code1 == "non organic"|
           amendment_123 ==  "FertClay" |
           amendment_123 ==  "ClFertClay" |
           amendment_123 ==  "ClClay" )




Handndorf_rip_AmendClay_box_plot <- Handndorf_rip_AmendClay %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Acidity)

Handndorf_rip_AmendClay_box_plot




summary_Handndorf_rip_AmendClay <-
  Handndorf_rip_AmendClay %>%
  dplyr::group_by(Acidity,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_AmendClay

################################################################################
unique(Handndorf_spading$soil_modification)
unique(Handndorf_spading$Acidity)
Handndorf_spading



Handndorf_spading_box_plot <- Handndorf_spading %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Acidity)

Handndorf_spading_box_plot




summary_Handndorf_spading <-
  Handndorf_spading %>%
  dplyr::group_by(Acidity,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_spading


##############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
Handndorf_spading_ave_yield <- Handndorf_spading %>% 
  group_by(site, yr_post_amelioration, Acidity) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
Handndorf_spading_ave_yield <- Handndorf_spading_ave_yield %>%
  group_by(site, Acidity) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
Handndorf_rip_no_amend_ave_yield
View(Handndorf_rip_no_amend_ave_yield)

Handndorf_spading_ave_yield_plot <- Handndorf_spading_ave_yield %>% 
  group_by(yr_post_amelioration, Acidity) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
Handndorf_spading_ave_yield_plot

Handndorf_spading_ave_yield_plot <- Handndorf_spading_ave_yield_plot %>% 
  mutate(constraint = "Acidity",
         treatment = "Spade")
Handndorf_spading_ave_yield_plot <- Handndorf_spading_ave_yield_plot %>% 
  arrange(Acidity, yr_post_amelioration)
Handndorf_spading_ave_yield_plot
Handndorf_spading_ave_yield_plot <- Handndorf_spading_ave_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

Handndorf_spading_ave_yield_plot

cum_Acid_Spade <- Handndorf_spading_ave_yield_plot
##############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
#merge
cum_Acid_Rip
cum_Acid_Spade


cum_Repl_options <- rbind(cum_Acid_Rip, cum_Acid_Spade)


###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################


cum_Repl_options %>%
  filter(yr_post_amelioration < 4) %>%
  filter(treatment!= "IncRip") %>% 
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(-2,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Acidity     )



##############################################################################

#get lime or clay we only had a few with clay eg Yenda but that was a sweep
#most of the clay was added with something else and is coded mixed
Handndorf_spade_Amend <- All_Handndorf %>%  filter(soil_modification == "Spade")


unique(Handndorf_spade_Amend$amendment_code1)
Handndorf_spade_Amend_clay <- Handndorf_spade_Amend %>% 
  filter(amendment_code1 == "non organic"|
           amendment_123 ==  "FertClay" |
           amendment_123 ==  "ClFertClay" |
           amendment_123 ==  "ClClay" )

Handndorf_spade_Amend_clay$Acidity <- factor(
  Handndorf_spade_Amend_clay$Acidity,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)


Handndorf_spade_Amend_clay_box_plot <- Handndorf_spade_Amend_clay %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Acidity)

Handndorf_spade_Amend_clay_box_plot




summary_Handndorf_spade_Amend_clay <-
  Handndorf_spade_Amend_clay %>%
  dplyr::group_by(Acidity,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_spade_Amend_clay




################################################################################
## stuff I am not suppose to talk about.

All_Handndorf <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/Jackie/summary_control_data_all2023-03-02.csv")
str(All_Handndorf)
All_Handndorf %>%  distinct(Descriptors ) %>%  arrange(Descriptors)

unique(Handndorf_rip_Amend$amendment_code1)

Handndorf_rip_Amend <- All_Handndorf %>%  filter(soil_modification == 'Rip')
Handndorf_rip_Amend <- Handndorf_rip_Amend %>%  filter(amendment_code1 == 'animal'|
                                                amendment_code1 == 'plant'| 
                                                amendment_code1 == 'fertiliser' )

################################################################################
### keep sites with Moderate / Severe soil strength

Handndorf_rip_Amend$Physical <- factor(
  Handndorf_rip_Amend$Physical,
  levels = c(0, 1, 2),
  labels = c("No issue",
             "Moderate issue",
             "Severe issue")
)
unique(Handndorf_rip_Amend$Physical)
str(Handndorf_rip_Amend)
unique(Handndorf_rip_Amend$soil_modification)



###############################################################################

###############################################################################

Handndorf_rip_Amend_box_plot <- Handndorf_rip_Amend %>%  
  filter(!is.na(yr_post_amelioration )) %>% 
  filter(yr_post_amelioration < 4 ) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2
  )+
  scale_y_continuous(limits=c(-2,3))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text=element_text(size=12),
         strip.text.x = element_text(size = 30))+
  labs(x = "Years post treatment", y = "Yield gain t/ha")+
  facet_wrap(.~Physical)

Handndorf_rip_Amend_box_plot




summary_Handndorf_rip_Amend <-
  Handndorf_rip_Amend %>%
  dplyr::group_by(Physical,
                  yr_post_amelioration) %>%
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE),
                   n= n())


summary_Handndorf_rip_Amend


###############################################################################
## cum yield for soil strength rip no amend ##
###############################################################################
## Average and summaries the data
#each site each year what is the average yld and constraints
soil_strength_rip_amend_ave_yield <- Handndorf_rip_Amend %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

## cumulative yield by site
soil_strength_rip_amend_ave_yield <- soil_strength_rip_amend_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))
soil_strength_rip_amend_ave_yield


soil_strength_rip_amend_ave_yield_plot <- soil_strength_rip_amend_ave_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean),
    count = n())
soil_strength_rip_amend_ave_yield_plot

soil_strength_rip_amend_ave_yield_plot <- soil_strength_rip_amend_ave_yield_plot %>% 
  mutate(constraint = "soil strength",
         treatment = "Rip with Amendments")
soil_strength_rip_amend_ave_yield_plot <- soil_strength_rip_amend_ave_yield_plot %>% 
  arrange(Physical, yr_post_amelioration)
soil_strength_rip_amend_ave_yield_plot
soil_strength_rip_amend_ave_yield_plot <- soil_strength_rip_amend_ave_yield_plot %>% 
  filter(yr_post_amelioration < 4) %>% 
  filter(count >1)

soil_strength_rip_amend_ave_yield_plot
cum_SS_rip_amendments <- soil_strength_rip_amend_ave_yield_plot
###############################################################################
## PLOT cum yield for soil strength rip no amend ##
###############################################################################
cum_SS_rip
cum_SS_rip_amendments

cum_SS_otions1 <- rbind(cum_SS_rip, cum_SS_rip_amendments)

cum_SS_otions1 %>%
  filter(yr_post_amelioration < 4) %>%
  ggplot(aes(x = yr_post_amelioration, y = cum_yld_mean, group = treatment)) +
  geom_line(aes(color=treatment), linewidth = 2)+
  #geom_point(aes(color=treatment))+
  scale_color_brewer(palette="Set2")+ 
  theme_bw()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=18),
         axis.text.x = element_text(size=18),
         legend.text = element_text(size=20),
         legend.title = element_blank(),
         legend.position = "bottom" ,
         strip.text.x = element_text(size = 30))+
  labs(#title = "Constraint = Repellence",
    x = "Years post inclusion treatment",
    y = "Mean Cumulative Yield Gain t/ha")+
  facet_wrap(.~Physical)
