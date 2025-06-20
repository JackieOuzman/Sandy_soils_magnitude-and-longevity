### Plots for GRDC updates paper 20203
## Run the step4 control yldsREVISED.Rmd - step by step so you have access to the df

### run to about 990 code chuck called "box and spading ripping only Agro Conf talk"

str(summary_control_data_all)




rm(list = ls()[!ls() %in% c("summary_control_data_all")])



#########################################################################################################################################
#### new plot - look see as of 11/1/2023
#### oh I need the soil constraint data - bugger



#########################################################################################################################################
### prep the data ####
#########################################################################################################################################
soil_constraints <- read.csv("X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_merged_all_messy.csv")
names(soil_constraints)

#keep a subset of data
soil_constraints <- soil_constraints %>% 
  dplyr::select(site, Repellence, Acidity, Physical, Nutrient, other )

#keep a data for sites only - remove the duplicates
soil_constraints <- soil_constraints %>%
  dplyr::distinct(site, .keep_all = TRUE)


### join this to the summary_control_data_all data

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

#########################################################################################################################################
### plots for TB Version 1 11/1/2023####
#########################################################################################################################################

#########################################################################################################################################
### 1. Spading only with Water Repellence
#########################################################################################################################################


summary_control_data_all_soil_constraints$Repellence <- factor(summary_control_data_all_soil_constraints$Repellence,
                                                                  levels = c(0,1,2),
                                                                  labels = c("No issue", 
                                                                             "Moderate issue", 
                                                                             "Severe issue"))

unique(summary_control_data_all_soil_constraints$Repellence)
#### plot option 1 - box plot years post
spade_box_Repellence_1 <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Spade") %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
               )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Spading",
       x = "Years post spading", y = "Yield gain t/ha")+
  facet_wrap(.~ Repellence)

spade_box_Repellence_1

#### plot option 2


summary_control_Repellence_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Spade") %>% 
  dplyr::group_by(Repellence, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Repellence_summary_2

spade_bar_Repellence_mean_2 <- summary_control_Repellence_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
   theme (title = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.title.x = element_text(size = 14),
          axis.text=element_text(size=12))+
  labs(#title = "Spading",
       x = "Years post spading", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Repellence)

spade_bar_Repellence_mean_2

#### plot option 3


summary_control_Repellence_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Spade") %>% 
  dplyr::group_by(Repellence ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



spade_bar_Repellence_mean_all_years_3 <- summary_control_Repellence_summary_all_years_3 %>%  
  ggplot( mapping = aes(Repellence     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Spading \nConstraint = Water Repllence",
       x = "",
       y = "Mean Yield gain t/ha")

spade_bar_Repellence_mean_all_years_3




spading_with_Repllence <- ggarrange(spade_bar_Repellence_mean_all_years_3, spade_bar_Repellence_mean_2, spade_box_Repellence_1, ncol = 2, nrow = 2)
spading_with_Repllence



ggsave(spading_with_Repllence,
       device = "png",
       filename = "spading_with_Repllence.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 2. Spading only with Acid
#########################################################################################################################################
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

summary_control_data_all_soil_constraints$Acidity <- factor(summary_control_data_all_soil_constraints$Acidity,
                                                               levels = c(0,1,2),
                                                               labels = c("No issue", 
                                                                          "Moderate issue", 
                                                                          "Severe issue"))

unique(summary_control_data_all_soil_constraints$Acidity)
#### plot option 1 - box plot years post
spade_box_Acid_1 <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Spade") %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
  )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Spading",
    x = "Years post spading", y = "Yield gain t/ha")+
  facet_wrap(.~ Acidity)

spade_box_Acid_1

#### plot option 2


summary_control_Acid_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Spade") %>% 
  dplyr::group_by(Acidity, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Acid_summary_2

spade_bar_Acid_mean_2 <- summary_control_Acid_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Spading",
    x = "Years post spading", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Acidity)

spade_bar_Acid_mean_2

#### plot option 3


summary_control_Acid_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Spade") %>% 
  dplyr::group_by(Acidity ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



spade_bar_Acid_mean_all_years_3 <- summary_control_Acid_summary_all_years_3 %>%  
  ggplot( mapping = aes(Acidity     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Spading \nConstraint = Acidity",
       x = "",
       y = "Mean Yield gain t/ha")

spade_bar_Acid_mean_all_years_3




spading_with_Acidity <- ggarrange(spade_bar_Acid_mean_all_years_3, spade_bar_Acid_mean_2, spade_box_Acid_1, ncol = 2, nrow = 2)
spading_with_Acidity



ggsave(spading_with_Acidity,
       device = "png",
       filename = "spading_with_Acidity.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 3. Ripping only with soil strenghth
#########################################################################################################################################
names(summary_control_data_all_soil_constraints)
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)


summary_control_data_all_soil_constraints$Physical <- factor(summary_control_data_all_soil_constraints$Physical,
                                                            levels = c(0,1,2),
                                                            labels = c("No issue", 
                                                                       "Moderate issue", 
                                                                       "Severe issue"))

unique(summary_control_data_all_soil_constraints$Physical)
unique(summary_control_data_all_soil_constraints$soil_modification)

#### plot option 1 - box plot years post
Rip_box_Physical_1 <- summary_control_data_all_soil_constraints %>%  
  dplyr::filter(soil_modification == "Rip") %>% 
  filter(!is.na(Physical)) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
  )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post ripping", y = "Yield gain t/ha")+
  facet_wrap(.~ Physical)

Rip_box_Physical_1

#### plot option 2


summary_control_Phyical_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(Physical)) %>% 
  dplyr::group_by(Physical, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Phyical_summary_2

Rip_bar_Physical_mean_2 <- summary_control_Phyical_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post ripping", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Physical)

Rip_bar_Physical_mean_2

#### plot option 3


summary_control_Physical_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(Physical)) %>% 
  dplyr::group_by(Physical ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



Rip_bar_Physical_mean_all_years_3 <- summary_control_Physical_summary_all_years_3 %>%  
  ggplot( mapping = aes(Physical     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Ripping \nConstraint = Physical",
       x = "",
       y = "Mean Yield gain t/ha")

Rip_bar_Physical_mean_all_years_3




ripping_with_Physical <- ggarrange(Rip_bar_Physical_mean_all_years_3, Rip_bar_Physical_mean_2, Rip_box_Physical_1, ncol = 2, nrow = 2)
ripping_with_Physical



ggsave(ripping_with_Physical,
       device = "png",
       filename = "ripping_with_Physical.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 




#########################################################################################################################################
### 3. Ripping only with repllency
#########################################################################################################################################
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

names(summary_control_data_all_soil_constraints)


unique(summary_control_data_all_soil_constraints$Repellence)
unique(summary_control_data_all_soil_constraints$soil_modification)

#### plot option 1 - box plot years post
Rip_box_Repellence_1 <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(Repellence)) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
  )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post ripping", y = "Yield gain t/ha")+
  facet_wrap(.~ Repellence)

Rip_box_Repellence_1

#### plot option 2


summary_control_Repellence_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(Repellence)) %>% 
  dplyr::group_by(Repellence, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Repellence_summary_2

Rip_bar_Repellence_mean_2 <- summary_control_Repellence_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post ripping", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Repellence)

Rip_bar_Repellence_mean_2

#### plot option 3


summary_control_Repellence_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "Rip") %>% 
  filter(!is.na(Repellence)) %>% 
  dplyr::group_by(Repellence ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



Rip_bar_Repellence_mean_all_years_3 <- summary_control_Repellence_summary_all_years_3 %>%  
  ggplot( mapping = aes(Repellence     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Ripping \nConstraint = Repellence",
       x = "",
       y = "Mean Yield gain t/ha")

Rip_bar_Repellence_mean_all_years_3




ripping_with_Repellence <- ggarrange(Rip_bar_Repellence_mean_all_years_3, Rip_bar_Repellence_mean_2, Rip_box_Repellence_1, ncol = 2, nrow = 2)
ripping_with_Repellence



ggsave(ripping_with_Repellence,
       device = "png",
       filename = "ripping_with_Repellence.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 3. Inclusion Ripping only with repllency
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

names(summary_control_data_all_soil_constraints)


unique(summary_control_data_all_soil_constraints$Repellence)
unique(summary_control_data_all_soil_constraints$soil_modification)

#### plot option 1 - box plot years post
IncRip_box_Repellence_1 <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Repellence)) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
  )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post Inc ripping", y = "Yield gain t/ha")+
  facet_wrap(.~ Repellence)

IncRip_box_Repellence_1

#### plot option 2


summary_control_Repellence_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Repellence)) %>% 
  dplyr::group_by(Repellence, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Repellence_summary_2

IncRip_bar_Repellence_mean_2 <- summary_control_Repellence_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Inc Ripping",
    x = "Years post Inc ripping", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Repellence)

IncRip_bar_Repellence_mean_2

#### plot option 3


summary_control_Repellence_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Repellence)) %>% 
  dplyr::group_by(Repellence ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



IncRip_bar_Repellence_mean_all_years_3 <- summary_control_Repellence_summary_all_years_3 %>%  
  ggplot( mapping = aes(Repellence     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Inc Ripping \nConstraint = Repellence",
       x = "",
       y = "Mean Yield gain t/ha")

IncRip_bar_Repellence_mean_all_years_3




Incripping_with_Repellence <- ggarrange(IncRip_bar_Repellence_mean_all_years_3, IncRip_bar_Repellence_mean_2, IncRip_box_Repellence_1, ncol = 2, nrow = 2)
Incripping_with_Repellence



ggsave(ripping_with_Repellence,
       device = "png",
       filename = "Inc_ripping_with_Repellence.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 3. Inclusion Ripping only with acid
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

names(summary_control_data_all_soil_constraints)


unique(summary_control_data_all_soil_constraints$Acidity)
unique(summary_control_data_all_soil_constraints$soil_modification)

#### plot option 1 - box plot years post
IncRip_box_Acidity_1 <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Acidity)) %>% 
  ggplot( mapping = aes(yr_post_amelioration, yield_gain, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_boxplot(outlier.shape = NA,
               #alpha = 0.2  
  )+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Ripping",
    x = "Years post Inc ripping", y = "Yield gain t/ha")+
  facet_wrap(.~ Acidity)

IncRip_box_Acidity_1

#### plot option 2


summary_control_Acidity_summary_2 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Acidity)) %>% 
  dplyr::group_by(Acidity, yr_post_amelioration ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))

summary_control_Acidity_summary_2

IncRip_bar_Acidity_mean_2 <- summary_control_Acidity_summary_2 %>%  
  ggplot( mapping = aes(yr_post_amelioration, yield_gain_mean, group = as.factor(yr_post_amelioration))) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text=element_text(size=12))+
  labs(#title = "Inc Ripping",
    x = "Years post Inc ripping", y = "Mean Yield gain t/ha")+
  facet_wrap(.~ Acidity)

IncRip_bar_Acidity_mean_2

#### plot option 3


summary_control_Acidity_summary_all_years_3 <- summary_control_data_all_soil_constraints %>% 
  filter(soil_modification == "IncRip") %>% 
  filter(!is.na(Acidity)) %>% 
  dplyr::group_by(Acidity ) %>% 
  dplyr::summarise(yield_gain_mean = mean(yield_gain, na.rm = TRUE))



IncRip_bar_Acidity_mean_all_years_3 <- summary_control_Acidity_summary_all_years_3 %>%  
  ggplot( mapping = aes(Acidity     , yield_gain_mean)) +
  theme_bw()+
  geom_col()+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12,  angle=45,hjust=1))+
  labs(title = "Inc Ripping \nConstraint = Acidity",
       x = "",
       y = "Mean Yield gain t/ha")

IncRip_bar_Acidity_mean_all_years_3




Incripping_with_Acidity <- ggarrange(IncRip_bar_Acidity_mean_all_years_3, IncRip_bar_Acidity_mean_2, IncRip_box_Acidity_1, ncol = 2, nrow = 2)
Incripping_with_Acidity



ggsave(Incripping_with_Acidity,
       device = "png",
       filename = "Incripping_with_Acidity.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/",
       width=8.62,
       height = 6.28,
       dpi=600
) 




#########################################################################################################################################
### plots for TB Version 2 13/1/2023####
#########################################################################################################################################

#########################################################################################################################################
### 1. Spading only with Water Repellence
#########################################################################################################################################
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

summary_control_data_all_soil_constraints$Repellence <- factor(summary_control_data_all_soil_constraints$Repellence,
                                                               levels = c(0,1,2),
                                                               labels = c("No issue", 
                                                                          "Moderate issue", 
                                                                          "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Repellence)

spade_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Spade") 
names(spade_constraints)
spade_constraints <- spade_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                    Repellence, Acidity,           
                                    Physical, Nutrient,other)
unique(spade_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4


str(spade_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
spade_constraints_ave_yield <- spade_constraints %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

spade_constraints_ave_yield


## cumulative yield by site
spade_constraints_cum_yield <- spade_constraints_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

spade_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

spade_constraints_cum_yield_for_plot <- spade_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
         cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
         yld_sd = sd(cum_yld_site,na.rm=TRUE),
         yld_cv = (yld_sd / cum_yld_mean))

spade_constraints_cum_yield_for_plot
view(spade_constraints_cum_yield_for_plot)

#plot

#view(spade_constraints_cum_yield)

#view(spade_constraints_cum_yield_for_plot)


spade_Repellence_cum_yield_1 <- spade_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Repellence)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  
  scale_y_continuous(limits=c(-3,6))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Spading \nConstraint = Repellence",
       x = "Years post spading",
       y = "Mean Cumulative Yield Gain t/ha")

  spade_Repellence_cum_yield_1


  
ggsave(spade_Repellence_cum_yield_1,
         device = "png",
         filename = "spade_Repellence_cum_yield.png",
         path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
         width=8.62,
         height = 6.28,
         dpi=600
  ) 

spade_constraints_cum_yield_for_plot_yr3 <- spade_constraints_cum_yield_for_plot %>% 
  dplyr::filter(yr_post_amelioration <4)

spade_Repellence_cum_yield_yr3 <- spade_constraints_cum_yield_for_plot_yr3 %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Repellence)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  
  scale_y_continuous(limits=c(-3,6))+
  
  theme (title = element_text(size = 20), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=16),
         axis.text.x = element_text(size=16),
         strip.text.x = element_text(size = 20))+
  labs(title = "Spading \nConstraint = Repellence",
       x = "Years post spading",
       y = "Mean Cumulative Yield Gain t/ha")

spade_Repellence_cum_yield_yr3



ggsave(spade_Repellence_cum_yield_yr3,
       device = "png",
       filename = "spade_Repellence_cum_yield_yrs3.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 

#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(spade_constraints_cum_yield_for_plot_yr3, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/spade_Repellence_cum_yield_yr3.csv")

ggsave(spade_Repellence_cum_yield_yr3,
       device = "png",
       filename = "spade_Repellence_cum_yield_yrs3.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


spade_constraints_cum_yield_for_plot


spade_constraints_cum_yield_for_plot_yr3_mix <- spade_constraints_cum_yield_for_plot %>% 
  dplyr::filter(Repellence == "No issue" & yr_post_amelioration <4 | 
                Repellence == "Moderate issue" |
                Repellence == "Severe issue" & yr_post_amelioration <4  )

spade_Repellence_cum_yield_yr3_mix <- spade_constraints_cum_yield_for_plot_yr3_mix %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Repellence)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  
  scale_y_continuous(limits=c(-3,6))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Spading \nConstraint = Repellence",
       x = "Years post spading",
       y = "Mean Cumulative Yield Gain t/ha")

spade_Repellence_cum_yield_yr3_mix



ggsave(spade_Repellence_cum_yield_yr3_mix,
       device = "png",
       filename = "spade_Repellence_cum_yield_yrs3_mix.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600)




#step 1 spade_constraints
#step 2 spade_constraints_ave_yield - average yld per year since site and repellency (same as site)
#step 3 spade_constraints_cum_yield - add clm for cum yield
#step 4 spade_constraints_cum_yield_for_plot - group by year and repellency and cal the mean cum yld and sd and CV


write.csv(spade_constraints,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step1_spade_constraints.csv"))

write.csv(spade_constraints_ave_yield,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step2_spade_constraints_ave_yield.csv"))

write.csv(spade_constraints_cum_yield,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step3_spade_constraints_cum_yield.csv"))

write.csv(spade_constraints_cum_yield_for_plot,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step4_spade_constraints_cum_yield_for_plot.csv"))




#########################################################################################################################################
### 2. Spading only with acidity
#########################################################################################################################################
summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)

summary_control_data_all_soil_constraints$Acidity <- factor(summary_control_data_all_soil_constraints$Acidity,
                                                               levels = c(0,1,2),
                                                               labels = c("No issue", 
                                                                          "Moderate issue", 
                                                                          "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Acidity)

spade_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Spade") 
names(spade_constraints)
spade_constraints <- spade_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                          Repellence, Acidity,           
                                                          Physical, Nutrient,other)
unique(spade_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4


str(spade_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
spade_constraints_ave_yield <- spade_constraints %>% 
  group_by(site, yr_post_amelioration, Acidity) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

spade_constraints_ave_yield
spade_constraints_ave_yield <- spade_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)


### fill in the missing data with zero does not work because when you cal the cv its messed up 
spade_constraints_ave_yield$mean_yield_gain <- spade_constraints_ave_yield$mean_yield_gain %>% replace(is.na(.), 0)

#View(spade_constraints_ave_yield)

## cumulative yield by site
spade_constraints_cum_yield <- spade_constraints_ave_yield %>%
  group_by(site, Acidity) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

spade_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

## before I cal the CV I need to put the missing data back in so make an new clm
spade_constraints_cum_yield <- spade_constraints_cum_yield %>% 
  dplyr::mutate(cum_yld_site_with_NA = cum_yld_site)

spade_constraints_cum_yield <- spade_constraints_cum_yield %>% 
  dplyr::mutate(cum_yld_site_with_NA = case_when(
    mean_yield_gain != 0.00 ~ cum_yld_site,
    TRUE ~ NA_real_))
#View(spade_constraints_cum_yield)
str(spade_constraints_cum_yield)
    
spade_constraints_cum_yield_for_plot <- spade_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Acidity) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site_with_NA,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

spade_constraints_cum_yield_for_plot
#view(spade_constraints_cum_yield_for_plot)

#plot






spade_Acidity_cum_yield_1 <- spade_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  facet_wrap(.~Acidity)+
  scale_y_continuous(limits=c(-3,6))+
  scale_x_continuous(limits=c(0,3))+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Spading \nConstraint = Acidity",
       x = "Years post spading",
       y = "Mean Cumulative Yield Gain t/ha")

spade_Acidity_cum_yield_1



ggsave(spade_Acidity_cum_yield_1,
       device = "png",
       filename = "spade_Acidity_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(spade_constraints_cum_yield_for_plot, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/spade_constraints_acid_cum_yield_for_plot.csv")

ggsave(spade_Acidity_cum_yield_1,
       device = "png",
       filename = "spade_Acidity_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 2. Spading only with physical
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)
summary_control_data_all_soil_constraints$Physical <- factor(summary_control_data_all_soil_constraints$Physical,
                                                            levels = c(0,1,2),
                                                            labels = c("No issue", 
                                                                       "Moderate issue", 
                                                                       "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Physical)

spade_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Spade") 
names(spade_constraints)
spade_constraints <- spade_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                          Repellence, Acidity,           
                                                          Physical, Nutrient,other)
unique(spade_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4
unique(spade_constraints$Physical)

str(spade_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
spade_constraints_ave_yield <- spade_constraints %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

spade_constraints_ave_yield
spade_constraints_ave_yield <- spade_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)
 


#View(spade_constraints_ave_yield)

## cumulative yield by site
spade_constraints_cum_yield <- spade_constraints_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

spade_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

spade_constraints_cum_yield_for_plot <- spade_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

spade_constraints_cum_yield_for_plot
view(spade_constraints_cum_yield_for_plot)

#plot


spade_constraints_cum_yield_for_plot_no_issue_removed <- spade_constraints_cum_yield_for_plot %>% 
  filter(Physical != "No issue")



spade_Physical_cum_yield_1 <- spade_constraints_cum_yield_for_plot_no_issue_removed %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Physical)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  scale_y_continuous(limits=c(-3,6))+
  scale_x_continuous(limits=c(0,3))+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Spading \nConstraint = Physical",
       x = "Years post spading",
       y = "Mean Cumulative Yield Gain t/ha")

spade_Physical_cum_yield_1



ggsave(spade_Physical_cum_yield_1,
       device = "png",
       filename = "spade_Physical_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(spade_constraints_cum_yield_for_plot_no_issue_removed, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/spade_constraints_cum_yield_for_plot_no_issue_removed.csv")

ggsave(spade_Physical_cum_yield_1,
       device = "png",
       filename = "spade_Physical_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/",
       width=8.62,
       height = 6.28,
       dpi=600
) 





spade_constraints_cum_yield_for_plot_yr3 <- spade_constraints_cum_yield_for_plot_no_issue_removed %>% 
  dplyr::filter(yr_post_amelioration < 4)


spade_Physical_cum_yield_yr3 <- spade_constraints_cum_yield_for_plot_yr3 %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Physical)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Spading \nConstraint = Physical",
       x = "Years post spading",
       y = "Mean Cumlative Yield Gain t/ha")

spade_Physical_cum_yield_yr3



ggsave(spade_Physical_cum_yield_yr3,
       device = "png",
       filename = "spade_Physical_cum_yield_yr3.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 










#########################################################################################################################################
### 4. ripping only with physical
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)


summary_control_data_all_soil_constraints$Physical <- factor(summary_control_data_all_soil_constraints$Physical,
                                                             levels = c(0,1,2),
                                                             labels = c("No issue", 
                                                                        "Moderate issue", 
                                                                        "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Physical)

rip_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Rip") 
names(rip_constraints)
rip_constraints <- rip_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                          Repellence, Acidity,           
                                                          Physical, Nutrient,other)
unique(rip_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4
unique(rip_constraints$Physical)

## remove the missing data
rip_constraints <-rip_constraints %>%  filter(!is.na(yr_post_amelioration))
rip_constraints <-rip_constraints %>%  filter(!is.na(Physical))

str(rip_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
rip_constraints_ave_yield <- rip_constraints %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

rip_constraints_ave_yield
rip_constraints_ave_yield <- rip_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)



#View(spade_constraints_ave_yield)

## cumulative yield by site
rip_constraints_cum_yield <- rip_constraints_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

rip_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

rip_constraints_cum_yield_for_plot <- rip_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

rip_constraints_cum_yield_for_plot
#view(rip_constraints_cum_yield_for_plot)

#plot






rip_Physical_cum_yield_1 <- rip_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Physical)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Ripping \nConstraint = Physical",
       x = "Years post ripping",
       y = "Mean Cumlative Yield Gain t/ha")

rip_Physical_cum_yield_1



ggsave(rip_Physical_cum_yield_1,
       device = "png",
       filename = "rip_Physical_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 
rip_constraints_cum_yield_for_plot_3yrs <- rip_constraints_cum_yield_for_plot %>% 
  filter(yr_post_amelioration< 4)

rip_Physical_cum_yield_3yrs <- rip_constraints_cum_yield_for_plot_3yrs %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  facet_wrap(.~Physical)+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  theme (title = element_text(size = 20), 
         axis.title.y = element_text(size = 20), 
         axis.title.x = element_text(size = 20),
         axis.text.y = element_text(size=16),
         axis.text.x = element_text(size=16),
         strip.text.x = element_text(size = 20))+


  labs(title = "Ripping \nConstraint = Physical",
       x = "Years post ripping",
       y = "Mean Cumlative Yield Gain t/ha")

rip_Physical_cum_yield_3yrs



ggsave(rip_Physical_cum_yield_3yrs,
       device = "png",
       filename = "rip_Physical_cum_yield_3yrs.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


#########################################################################################################################################
### 5. ripping only with repllency - done extra formats
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)


summary_control_data_all_soil_constraints$Repellence <- factor(summary_control_data_all_soil_constraints$Repellence,
                                                             levels = c(0,1,2),
                                                             labels = c("No issue", 
                                                                        "Moderate issue", 
                                                                        "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Repellence)

rip_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "Rip") 
names(rip_constraints)
rip_constraints <- rip_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                      Repellence, Acidity,           
                                                      Physical, Nutrient,other)
unique(rip_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4
unique(rip_constraints$Repellence)

## remove the missing data
rip_constraints <-rip_constraints %>%  filter(!is.na(yr_post_amelioration))
rip_constraints <-rip_constraints %>%  filter(!is.na(Repellence))

str(rip_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
rip_constraints_ave_yield <- rip_constraints %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

rip_constraints_ave_yield
rip_constraints_ave_yield <- rip_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)



#View(spade_constraints_ave_yield)

## cumulative yield by site
rip_constraints_cum_yield <- rip_constraints_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

rip_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

rip_constraints_cum_yield_for_plot <- rip_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

rip_constraints_cum_yield_for_plot
#view(rip_constraints_cum_yield_for_plot)

#plot






rip_Repellence_cum_yield_1 <- rip_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  facet_wrap(.~Repellence)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Ripping \nConstraint = Repellence",
       x = "Years post ripping",
       y = "Mean Cumlative Yield Gain t/ha")

rip_Repellence_cum_yield_1

#step 1 rip_constraints
#step 2 rip_constraints_ave_yield - average yld per year since site and repellency (same as site)
#step 3 rip_constraints_cum_yield - add clm for cum yield
#step 4 rip_constraints_cum_yield_for_plot - group by year and repellency and cal the mean cum yld and sd and CV


write.csv(rip_constraints,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step1_rip_constraints.csv"))

write.csv(rip_constraints_ave_yield,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step2_rip_constraints_ave_yield_rep.csv"))

write.csv(rip_constraints_cum_yield,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step3_rip_constraints_cum_yield_rep.csv"))

write.csv(rip_constraints_cum_yield_for_plot,
          paste0("X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
                 "step4_rip_constraints_cum_yield_for_plot_rep.csv"))



ggsave(rip_Repellence_cum_yield_1,
       device = "png",
       filename = "rip_Repellence_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 

rip_constraints_cum_yield_for_plot_3 <- rip_constraints_cum_yield_for_plot %>% 
  filter(yr_post_amelioration <4)


rip_Repellence_cum_yield_3 <- rip_constraints_cum_yield_for_plot_3 %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  facet_wrap(.~Repellence)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv), colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv), colour = "grey")+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Ripping \nConstraint = Repellence",
       x = "Years post ripping",
       y = "Mean Cumlative Yield Gain t/ha")

rip_Repellence_cum_yield_3
ggsave(rip_Repellence_cum_yield_3,
       device = "png",
       filename = "rip_Repellence_cum_yield_3yrs.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#########################################################################################################################################
### 6. Inripping only with repllency - done extra formats
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)


summary_control_data_all_soil_constraints$Repellence <- factor(summary_control_data_all_soil_constraints$Repellence,
                                                               levels = c(0,1,2),
                                                               labels = c("No issue", 
                                                                          "Moderate issue", 
                                                                          "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Repellence)

Incrip_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "IncRip") 
names(Incrip_constraints)
Incrip_constraints <- Incrip_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                      Repellence, Acidity,           
                                                      Physical, Nutrient,other)
unique(Incrip_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4
unique(Incrip_constraints$Repellence)

#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(Incrip_constraints, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/Incrip_constraints_all.csv")

## remove the missing data
Incrip_constraints <-Incrip_constraints %>%  filter(!is.na(yr_post_amelioration))
Incrip_constraints <-Incrip_constraints %>%  filter(!is.na(Repellence))

str(Incrip_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
Incrip_constraints_ave_yield <- Incrip_constraints %>% 
  group_by(site, yr_post_amelioration, Repellence) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

Incrip_constraints_ave_yield
Incrip_constraints_ave_yield <- Incrip_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)



#View(spade_constraints_ave_yield)

## cumulative yield by site
Incrip_constraints_cum_yield <- Incrip_constraints_ave_yield %>%
  group_by(site, Repellence) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

Incrip_constraints_cum_yield
#View(Incrip_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

Incrip_constraints_cum_yield_for_plot <- Incrip_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Repellence) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

Incrip_constraints_cum_yield_for_plot
#view(Incrip_constraints_cum_yield_for_plot)

#plot






Incrip_Repellence_cum_yield_1 <- Incrip_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  facet_wrap(.~Repellence)+
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  
  scale_y_continuous(limits=c(-2,3))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Inclusion Ripping \nConstraint = Repellence",
       x = "Years post inclusion ripping",
       y = "Mean Cumulative Yield Gain t/ha")

Incrip_Repellence_cum_yield_1



ggsave(Incrip_Repellence_cum_yield_1,
       device = "png",
       filename = "Incrip_Repellence_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(Incrip_constraints_cum_yield_for_plot, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/Incrip_constraints_Repellence_cum_yield_for_plot.csv")

ggsave(Incrip_Repellence_cum_yield_1,
       device = "png",
       filename = "Incrip_Repellence_cum_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/",
       width=8.62,
       height = 6.28,
       dpi=600
) 


#########################################################################################################################################
### 7. Inripping only with physical - done extra formats
#########################################################################################################################################

summary_control_data_all_soil_constraints <- left_join(summary_control_data_all, soil_constraints)


summary_control_data_all_soil_constraints$Physical <- factor(summary_control_data_all_soil_constraints$Physical,
                                                               levels = c(0,1,2),
                                                               labels = c("No issue", 
                                                                          "Moderate issue", 
                                                                          "Severe issue"))
## subset the data
unique(summary_control_data_all_soil_constraints$Physical)

Incrip_constraints <- summary_control_data_all_soil_constraints %>%  
  filter(soil_modification == "IncRip") 
names(Incrip_constraints)
Incrip_constraints <- Incrip_constraints %>%  dplyr::select(ID, site, yr_post_amelioration, yield_gain, 
                                                            Repellence, Acidity,           
                                                            Physical, Nutrient,other)
unique(Incrip_constraints$yr_post_amelioration) #0, 1 ,2 ,3 ,4
unique(Incrip_constraints$Physical)

## remove the missing data
Incrip_constraints <-Incrip_constraints %>%  filter(!is.na(yr_post_amelioration))
Incrip_constraints <-Incrip_constraints %>%  filter(!is.na(Physical))

str(Incrip_constraints)

## Average and summaries the data


#each site each year what is the average yld and constraints
Incrip_constraints_ave_yield <- Incrip_constraints %>% 
  group_by(site, yr_post_amelioration, Physical) %>% 
  dplyr::summarise(mean_yield_gain = mean(yield_gain, na.rm = TRUE ))

Incrip_constraints_ave_yield
Incrip_constraints_ave_yield <- Incrip_constraints_ave_yield %>% 
  arrange(site, yr_post_amelioration)



#View(spade_constraints_ave_yield)

## cumulative yield by site
Incrip_constraints_cum_yield <- Incrip_constraints_ave_yield %>%
  group_by(site, Physical) %>%
  mutate(cum_yld_site = cumsum(mean_yield_gain))

Incrip_constraints_cum_yield
#View(spade_constraints_cum_yield)
## mean cumulative yield by year and cal the cv of the mean yld gain

Incrip_constraints_cum_yield_for_plot <- Incrip_constraints_cum_yield %>% 
  group_by(yr_post_amelioration, Physical) %>%
  dplyr::summarise(
    cum_yld_mean = mean(cum_yld_site , na.rm=TRUE),
    yld_sd = sd(cum_yld_site,na.rm=TRUE),
    yld_cv = (yld_sd / cum_yld_mean))

Incrip_constraints_cum_yield_for_plot
#view(Incrip_constraints_cum_yield_for_plot)

#plot






Incrip_Physical_yield_1 <- Incrip_constraints_cum_yield_for_plot %>%  
  ggplot( mapping = aes(yr_post_amelioration, cum_yld_mean)) +
  theme_bw()+
  geom_ribbon(aes(ymin = cum_yld_mean-yld_cv, ymax = cum_yld_mean+yld_cv), fill = "light grey", alpha = .5) +
  geom_line(colour = "Black", linewidth = 1.5)+
  
  geom_line(aes(yr_post_amelioration, cum_yld_mean+yld_cv),  colour = "grey")+
  geom_line(aes(yr_post_amelioration, cum_yld_mean-yld_cv),  colour = "grey")+
  facet_wrap(.~Physical)+
  
  scale_y_continuous(limits=c(-2,3))+
  scale_x_continuous(limits=c(0,3))+
  
  theme (title = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.title.x = element_text(size = 14),
         axis.text.y = element_text(size=12),
         axis.text.x = element_text(size=12))+
  labs(title = "Inclusion Ripping \nConstraint = Physical",
       x = "Years post inclusion ripping",
       y = "Mean Cumulative Yield Gain t/ha")

Incrip_Physical_yield_1



ggsave(Incrip_Physical_yield_1,
       device = "png",
       filename = "Incrip_Physical_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



#### additional saving for TB - she thinks there is a error - I cant see it??
write.csv(Incrip_constraints_cum_yield_for_plot, 
          "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/Incrip_constraints_physical_cum_yield_for_plot.csv")

ggsave(Incrip_Physical_yield_1,
       device = "png",
       filename = "Incrip_Physical_yield.png",
       path= "X:/Therese_Jackie/Sandy_soils/Development_database/for_presentations/GRDC_2023_Jan/cum_yld_gain_plots/Check_ for_TB/",
       width=8.62,
       height = 6.28,
       dpi=600
) 



